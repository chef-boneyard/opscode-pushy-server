# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
#
# Author:: Steven Grady (<steven.grady@erlang-solutions.com>)

# @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require 'httpclient'
require 'pushy/spec_helper'
require 'time'

def d(s)
    p "#{Time.now}: #{s}"
end

describe "sse-test" do
#describe "sse-test", :focus=>true do
  include_context "end_to_end_util"     # to start clients
  SUMMARY_WAIT_TIME = 5
  JOB_WAITING_AROUND_TIME = 60
  let(:command) { 'sleep 1' }
  let(:quorum) { 1 }
  let(:run_timeout) { 2 }
  def job_to_run                        # use "def", because "let" caches, and we want to always get the latest version
    {
      'command' => command,
      'nodes' => nodes,
      'run_timeout' => run_timeout
    }
  end

  after :each do
    if @clients
      @clients.each do |client_name, client|
        stop_client(client_name) if @clients[client_name][:client]
      end
      @clients = nil
    end
  end

  let(:feed_path) { "/pushy/job_status_feed"}

  def get_feed(id, &block)
    # SLG - things included via chef-pedant/lib/pedant/rspec/common.rb
    # SLG - "get" defined in chef-pedant/lib/pedant/request.rb
    # SLG - "api_url" defined in oc-chef-pedant/lib/pedant/multitenant/platform.rb
    # SLG = admin_user defined in rspec/common.rb
    feed_url = api_url("#{feed_path}/#{id}")
    headers = {'Accept' => 'text/event-stream'}
    get(feed_url, admin_user, :headers => headers, &block)
  end

  def uri_from_id(id)
    api_url("/pushy/jobs/#{id}")
  end

  def start_new_job(job)
    uri = post(api_url("/pushy/jobs"), admin_user, :payload => job) do |response|
      list = JSON.parse(response.body)
      list["uri"]
    end
    job_info_js = get(uri, admin_user)
    job_info = JSON.parse(job_info_js)
    job_info['id']
  end

  # Modified from end_to_end_util:wait_for_job_status
  def wait_for_job_done(uri, options = {})
    job = nil
    begin
      Timeout::timeout(options[:timeout] || JOB_STATUS_TIMEOUT_DEFAULT) do
        begin
          sleep(SLEEP_TIME) if job
          job = get_job(uri)
        end until ['complete', 'timed_out', 'quorum_failed'].include?(job['status'])
      end
    rescue Timeout::Error
      raise "Job never got a status -- actual job reported as #{job}"
    end
    job
  end

  def do_complete_job(job, options = {})
    @id = start_new_job(job)
    wait_for_job_done(uri_from_id(@id), options)
    @id
  end

  def start_event_stream
    feed_url = api_url("#{feed_path}/#{@id}")
    feed_url.sub!("https://api.opscode.piab", "http://api.opscode.piab:10003")
    host = URI.parse(feed_url).host
    @evs = []
    c = HTTPClient.new
    # Certificate is self-signing -- if we don't disable certificate verification, this won't work
    c.ssl_config.verify_mode = OpenSSL::SSL::VERIFY_NONE
    @piper, @pipew = IO.pipe

    auth_headers = admin_user.signing_headers(:GET, feed_url, "")
    require 'chef/version'
    headers =
      {
        'Accept' => 'text/event-stream',
        'User-Agent' => 'chef-pedant rspec tests',
        'X-Chef-Version' => Chef::VERSION,
        'Host' => host
      }
    headers = headers.merge(auth_headers)
    
    @ep = EventParser.new
    Thread.new {
      conn = c.get_async(feed_url, :header => headers)
      resp = conn.pop
      content_io = resp.content
      while ! conn.finished?
        str = content_io.readpartial(4096)
        if str
          @pipew.write(str)
        end
      end
      @pipew.close
    }
  end

  def get_streaming_events
    evstr = @piper.readpartial(65536)
    @ep.feed(evstr)
    @evs += @ep.events_so_far
    @evs
  end

  def check_node(e, node)
    jnode = e.json['node']
    if (node != :any)
      jnode.should == node
    end
    jnode
  end

  def expect_start(e, command, run_timeout, quorum, username)
    e.name.should == :start
    e.json['command'].should == command
    e.json['run_timeout'].should == run_timeout
    e.json['quorum'].should == quorum
    e.json['user'].should == username
  end

  def expect_quorum_vote(e, node, status)
    e.name.should == :quorum_vote
    e.json['status'].should == status
    check_node(e, node)
  end

  def expect_quorum_succeeded(e)
    e.name.should == :quorum_succeeded
  end

  def expect_run_start(e, node)
    e.name.should == :run_start
    check_node(e, node)
  end

  def expect_run_complete(e, node, status)
    e.name.should == :run_complete
    e.json['status'].should == status
    check_node(e, node)
  end

  def expect_job_complete(e, status)
    e.name.should == :job_complete
    e.json['status'].should == status
  end

  def expect_summary(e, command, status, run_timeout, succeeded, failed)
    e.name.should == :summary
    e.json['command'].should == command
    e.json['status'].should == status
    e.json['run_timeout'].should == run_timeout
    created_at = Time.parse(e.json['created_at'])
    updated_at = Time.parse(e.json['updated_at'])
    updated_at.should >= created_at
    e.json['nodes']['succeeded'].should == succeeded
    e.json['nodes']['failed'].should == failed
  end

  # Adapted from github.com/conjurinc/sse-client-ruby -- should really just use that package;
  # but based on code inspection, it will miss the last event if it doesn't end with a "\n\n".
  # I suspect it was assuming an infinite stream.
  class Event < Struct.new(:data, :name, :id, :json); end
  class EventParser
    def initialize
      @buffer = ''
      @events = []
    end

    attr_reader :events

    def feed(chunk, final = false)
      @buffer << chunk
      process_events(final)
    end

    def process_events(final)
      while i = @buffer.index("\n\n")
        process_event(@buffer.slice!(0..i))
      end
      if final
        process_event(@buffer)
      end
    end

    def process_event(evstr)
      data, id, name = [], nil, nil
      evstr.lines.map(&:chomp).each do |l|
        field, value = case l
          when /^:/ then
            next # comment, do nothing
          when /^(.*?):(.*)$/ then
            [$1, $2]
          else
            [l, ''] # this is what the spec says, I swear!
        end
        # spec allows one optional space after the colon
        value = value[1..-1] if value.start_with? ' '
        case field
          when 'data' then
            data << value
          when 'id' then
            id = value
          when 'event' then
            name = value.to_sym
          when 'retry' then
            @retry = value.to_i
        end
      end
      @last_event_id = id
      @events << Event.new(data.join("\n"), name, id)
    end

    def events_so_far
      evs = @events
      @events = []
      evs
    end
  end

  def parse_stream(s)
    ep = EventParser.new()
    ep.feed(s, true)
    ep.events
  end

  # Validate standard events; as a side-effect, save the parsed json field
  def validate_events(numEvents, evs)
    d evs
    evs.length.should == numEvents
    # All ids are unique
    evs.map(&:id).uniq.length.should == evs.length
    # All events have (parsable) json for data
    evs.each do |e|
      e.json = JSON.parse(e.data)
    end
    require 'pp'
    pp evs
    # All events have (parsable) timestamps
    ts = evs.map {|e| Time.parse(e.json['timestamp'])}
    # All timestamps are unique
    ts.uniq.length.should == ts.length
    # All timestamps are in increasing order
    ts.sort.should == ts
    # All timestamps are in (roughly) increasing order -- since pushy-server
    # is multi-threaded, there may be some events showing up before others,
    # but that's okay as long as delta is very small.
    #ts.each_index do |i|
      #next if i == 0
      ## Check that time is forward, or no more than 1ms backward
      #(ts[i] - ts[i-1]).should > -0.001
    #end
  end

  def expect_valid_response(numEvents, response)
    response.should look_like({ :status => 200 })
    evs = parse_stream(response.body)
    validate_events(numEvents, evs)
    evs
  end

  context 'with no job,' do
    it "should respond to a non-existent job with a 404" do
      get_feed('nonexistent') do |response|
        response.should look_like({ :status => 404 })
      end
    end

    it "should respond to a POST with a 405" do
      post(api_url("#{feed_path}/nonexistent"), admin_user) do |response|
        response.should look_like({ :status => 405 })
      end
    end

    it 'should respond to an invalid user with a 401' do
      get(api_url("#{feed_path}/nonexistent"), invalid_user) do |response|
      response.should look_like({ :status => 401 })
      end
    end
  end

  context 'with a job with one nonexistent node,' do
    let(:node) { 'nonexistent' }
    let(:nodes) { [node] }

    before :each do
      do_complete_job(job_to_run)
    end

    it "the events should be: start,quorum_vote(down),job_complete(quorum_failed)" do
      get_feed(@id) do |response|
        evs = expect_valid_response(3, response)
        expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
        expect_quorum_vote(evs[1], node, 'down')
        expect_job_complete(evs[2], "quorum_failed")
      end
    end
  end

  context 'with a job with one node,' do
    let(:node) { 'DONKEY' }
    let(:nodes) { [node] }

    context "when the command succeeds" do
      before :each do
        start_new_clients(node)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_start,run_complete(success),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(6, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_run_start(evs[3], node)
          expect_run_complete(evs[4], node, 'success')
          expect_job_complete(evs[5], "complete")
        end
      end
    end

    context "when the command fails" do
      let(:command) { 'this_oughta_fail' }
      before :each do
        start_new_clients(node)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_start,run_complete(failure),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(6, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_run_start(evs[3], node)
          expect_run_complete(evs[4], node, 'failure')
          expect_job_complete(evs[5], "complete")
        end
      end
    end

    context "when the node nacks the quorum" do
      let(:command) { 'bad command' }
      before :each do
        start_new_clients(node)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(failure),job_complete(quorum_failed)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(3, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'failure')
          expect_job_complete(evs[2], "quorum_failed")
        end
      end
    end

    # XX This test takes a minute or so to run.  We need a way of telling the server to time
    # out more quickly.
    context "when the node ignores the quorum", :slow => true do
      before :each do
        class PushyClient
          alias old_commit commit
          def commit(job_id, command); nil; end
        end
        start_new_clients(node)
        do_complete_job(job_to_run, {:timeout => JOB_WAITING_AROUND_TIME+1})
      end

      after :each do
        class PushyClient
          alias commit old_commit
        end
      end

      # It would be nice if pushy-server sent something different here, maybe "commit_timeout" or something
      it "the events should be: start,quorum_vote(voting_timeout),job_complete(quorum_failed)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(3, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'voting_timeout')
          expect_job_complete(evs[2], "quorum_failed")
        end
      end
    end

    context "when the command nacks the run" do
      before :each do
        class PushyClient
          alias old_run run
          def run(job_id)
            self.send_command(:nack_run, job_id)
          end
        end
        start_new_clients(node)
        do_complete_job(job_to_run)
      end

      after :each do
        class PushyClient
          alias run old_run
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_complete(run_nacked),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(5, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_run_complete(evs[3], node, 'run_nacked')
          expect_job_complete(evs[4], "complete")
        end
      end
    end

    context "when the command times out" do
      before :each do
        class PushyClient
          alias old_run run
          def run(job_id); nil; end
        end
        start_new_clients(node)
        do_complete_job(job_to_run)
      end

      after :each do
        class PushyClient
          alias run old_run
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,job_complete(timed_out)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(4, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_job_complete(evs[3], "timed_out")
        end
      end
    end
  end

  context 'with a job with two nodes,' do
    let(:quorum) { 2 }
    let(:nodes) { ['DONKEY', 'FIONA'] }

    context "when the command succeeds on both" do
      before :each do
        start_new_clients(*nodes)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(A,success),quorum_vote(B,success),quorum_succeeded,run_start(A),run_start(B),run_complete(A,success),run_complete(B,success),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          n1 = expect_quorum_vote(evs[1], :any, 'success')
          n2 = expect_quorum_vote(evs[2], :any, 'success')
          [n1, n2].sort.should == nodes.sort
          expect_quorum_succeeded(evs[3])
          n1 = expect_run_start(evs[4], :any)
          n2 = expect_run_start(evs[5], :any)
          [n1, n2].sort.should == nodes.sort
          n1 = expect_run_complete(evs[6], :any, 'success')
          n2 = expect_run_complete(evs[7], :any, 'success')
          [n1, n2].sort.should == nodes.sort
          expect_job_complete(evs[8], "complete")
        end
      end
    end
    context "when the command fails on both" do
      let(:command) { 'this_oughta_fail' }
      before :each do
        start_new_clients(*nodes)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(A,success),quorum_vote(B,success),quorum_succeeded,run_start(A),run_start(B),run_complete(A,failure),run_complete(B,failure),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          n1 = expect_quorum_vote(evs[1], :any, 'success')
          n2 = expect_quorum_vote(evs[2], :any, 'success')
          [n1, n2].sort.should == nodes.sort
          expect_quorum_succeeded(evs[3])
          n1 = expect_run_start(evs[4], :any)
          n2 = expect_run_start(evs[5], :any)
          [n1, n2].sort.should == nodes.sort
          n1 = expect_run_complete(evs[6], :any, 'failure')
          n2 = expect_run_complete(evs[7], :any, 'failure')
          [n1, n2].sort.should == nodes.sort
          expect_job_complete(evs[8], "complete")
        end
      end
    end

    context "when the command fails on one" do
      before :each do
        start_new_clients(*nodes)
        # Do some ugly object hacking to get the clients to behave differently
        donkey = @clients['DONKEY'][:client]
        jr = donkey.instance_variable_get('@job_runner')
        class <<jr
          alias real_run run
          def run(job_id)
            @command = 'this_oughta_fail'
            real_run(job_id)
          end
        end
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(A,success),quorum_vote(B,success),quorum_succeeded,run_start(A),run_start(B),run_complete(A,failure),run_complete(B,success),job_complete(complete)" do
        get_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
          n1 = expect_quorum_vote(evs[1], :any, 'success')
          n2 = expect_quorum_vote(evs[2], :any, 'success')
          [n1, n2].sort.should == nodes.sort
          expect_quorum_succeeded(evs[3])
          n1 = expect_run_start(evs[4], :any)
          n2 = expect_run_start(evs[5], :any)
          [n1, n2].sort.should == nodes.sort
          if (evs[6].json['node'] == 'DONKEY') then
            de, fe = evs[6], evs[7]
          else
            fe, de = evs[6], evs[7]
          end
          expect_run_complete(de, 'DONKEY', 'failure')
          expect_run_complete(fe, 'FIONA', 'success')
          expect_job_complete(evs[8], "complete")
        end
      end
    end

    context "when the one rejects the quorum," do
      before :each do
        start_new_clients(*nodes)
        # Do some ugly object hacking to get the clients to behave differently
        donkey = @clients['DONKEY'][:client]
        jr = donkey.instance_variable_get('@job_runner')
        class <<jr
          def commit(job_id, command)
            client.send_command(:nack_commit, job_id)
            return false
          end
        end
        job = job_to_run.merge({'quorum' => quorum})
        do_complete_job(job)
      end

      context "when the quorum is 100%" do
        it "the events should be: start,quorum_vote(A,success),quorum_vote(B,failure),job_complete(quorum_failed)" do
          get_feed(@id) do |response|
            evs = expect_valid_response(4, response)
            expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
            if (evs[1].json['node'] == 'DONKEY') then
              de, fe = evs[1], evs[2]
            else
              fe, de = evs[1], evs[2]
            end
            expect_quorum_vote(de, 'DONKEY', 'failure')
            expect_quorum_vote(fe, 'FIONA', 'success')
            expect_job_complete(evs[3], "quorum_failed")
          end
        end
      end

      context "when the quorum is 50%" do
        let(:quorum) { 1 }
        it "the events should be: start,quorum_vote(A,success),quorum_vote(B,failure),quorum_succeeded,run_complete(A,success),job_complete(success)" do
          get_feed(@id) do |response|
            evs = expect_valid_response(7, response)
            expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
            if (evs[1].json['node'] == 'DONKEY') then
              de, fe = evs[1], evs[2]
            else
              fe, de = evs[1], evs[2]
            end
            expect_quorum_vote(de, 'DONKEY', 'failure')
            expect_quorum_vote(fe, 'FIONA', 'success')
            expect_quorum_succeeded(evs[3])
            expect_run_start(evs[4], 'FIONA')
            expect_run_complete(evs[5], 'FIONA', 'success')
            expect_job_complete(evs[6], "complete")
          end
        end
      end
    end
  end

  context "with a job that has been complete for a while," do
    let(:nodes) { ['DONKEY'] }
    before :each do
      start_new_clients(*nodes)
      do_complete_job(job_to_run)
      sleep(SUMMARY_WAIT_TIME + 1)
    end

    it "the response to a feed request should just be the summary of the job" do
      get_feed(@id) do |response|
        evs = expect_valid_response(1, response)
        expect_summary(evs[0], command, 'complete', run_timeout, ['DONKEY'], nil)
      end
    end
  end

  context 'with a job with with a non-ASCII name,' do
    let(:command) { "echo '\u0110ONKEY'"}
    let(:node) { 'DONKEY' }
    let(:nodes) { [node] }

    before :each do
      start_new_clients(node)
      donkey = @clients['DONKEY'][:client]
      donkey.instance_variable_get('@whitelist').instance_variable_set('@whitelist', {command => command})
      do_complete_job(job_to_run)
    end

    it "the events should be cleanly encoded" do
      get_feed(@id) do |response|
        evs = expect_valid_response(6, response)
        expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
        expect_quorum_vote(evs[1], node, 'success')
        expect_quorum_succeeded(evs[2])
        expect_run_start(evs[3], node)
        expect_run_complete(evs[4], node, 'success')
        expect_job_complete(evs[5], "complete")
      end
    end
  end

  context "with when reading an event stream,", :focus=>true do
    let(:node) { 'DONKEY' }
  let(:command) { 'sleep 5' }
  let(:run_timeout) { 10 }
    let(:nodes) { [node] }
    before :each do
      start_new_clients(node)
      @id = start_new_job(job_to_run)
      start_event_stream
    end

    it "the events become available as they happen" do
      evs = get_streaming_events
      validate_events(4, evs)
      expect_start(evs[0], command, run_timeout, quorum, admin_user.name)
      expect_quorum_vote(evs[1], node, 'success')
      expect_quorum_succeeded(evs[2])
      expect_run_start(evs[3], node)
      sleep 5
      evs = get_streaming_events
      validate_events(6, evs)
      expect_run_start(evs[3], node)
      expect_run_complete(evs[4], node, 'success')
      expect_job_complete(evs[5], "complete")
    end
  end
end
