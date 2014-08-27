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

require 'pushy/spec_helper'
require 'time'

describe "sse-test" do
  include_context "end_to_end_util"     # to start clients
  include_context "sse_support"
  SUMMARY_WAIT_TIME = 5
  JOB_WAITING_AROUND_TIME = 60
  let(:command) { 'sleep 1' }
  let(:quorum) { 1 }
  let(:run_timeout) { 2 }
  def job_to_run                        # use "def", because "let" caches, but we want to always get the latest version
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

  let(:job_feed_path) { "/pushy/job_status_feed"}

  context 'with no job,' do
    it "should respond to a non-existent job with a 404" do
      get_job_feed('nonexistent') do |response|
        response.should look_like({ :status => 404 })
      end
    end

    it "should respond to a POST with a 405" do
      post(api_url("#{job_feed_path}/nonexistent"), admin_user) do |response|
        response.should look_like({ :status => 405 })
      end
    end

    it 'should respond to an invalid user with a 401' do
      get(api_url("#{job_feed_path}/nonexistent"), invalid_user) do |response|
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

    it "the job events should be: start,quorum_vote(down),rehab,job_complete(quorum_failed)" do
      get_job_feed(@id) do |response|
        evs = expect_valid_response(4, response)
        expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
        expect_quorum_vote(evs[1], node, 'down')
        expect_rehab(evs[2], node)
        expect_job_complete(evs[3], "quorum_failed")
      end
    end

    ## XX can't easily write org-feed test, because neither httpclient nor typhoeus provides
    # any reasonable way to close a connection; and since the org feed never closes, things
    # # would be ugly.  Instead, the org-feed tests are written in the Erlang eunit suite for
    # pushy-server.
    #it "the org events should be: start,job_complete(quorum_failed)" do
      #get_org_feed do |response|
        #evs = expect_valid_response(2, response)
        #expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name, @id)
        #expect_job_complete(evs[2], "quorum_failed", @id)
      #end
    #end
  end

  context 'with a job with one node,' do
    let(:node) { 'DONKEY' }
    let(:nodes) { [node] }

    context "when the command succeeds" do
      before :each do
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      it 'should respond to an non-event-stream request with a 406' do
        # default is application/json
        get(api_url("#{job_feed_path}/#{@id}"), admin_user) do |response|
          response.should look_like({ :status => 406 })
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_start,run_complete(success),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(6, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
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
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_start,run_complete(failure),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(6, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
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
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(failure),job_complete(quorum_failed)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(3, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
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
          def commit(job_id, command, opts); nil; end
        end
        start_new_clients([node])
        do_complete_job(job_to_run, {:timeout => JOB_WAITING_AROUND_TIME+5})
      end

      after :each do
        class PushyClient
          alias commit old_commit
        end
      end

      # It would be nice if pushy-server sent something different here, maybe "commit_timeout" or something
      it "the events should be: start,quorum_vote(voting_timeout),job_complete(quorum_failed)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(3, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
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
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      after :each do
        class PushyClient
          alias run old_run
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,rehab,run_complete(run_nacked),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(6, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_rehab(evs[3], node)
          expect_run_complete(evs[4], node, 'run_nacked')
          expect_job_complete(evs[5], "complete")
        end
      end
    end

    context "when the command times out" do
      before :each do
        class PushyClient
          alias old_run run
          def run(job_id); nil; end
        end
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      after :each do
        class PushyClient
          alias run old_run
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,job_complete(timed_out)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(4, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_job_complete(evs[3], "timed_out")
        end
      end
    end

    context "when a buggy client sends a nack_run when already running," do
      before :each do
        class PushyClient
          alias old_run run
          def run(job_id)
            p "RUN #{job_id}"
            self.send_command(:ack_run, job_id)
            self.send_command(:nack_run, job_id)
          end
        end
        start_new_clients([node])
        do_complete_job(job_to_run)
      end

      after :each do
        class PushyClient
          alias run old_run
        end
      end

      it "the events should be: start,quorum_vote(success),quorum_succeeded,run_start,rehab,run_complete(run_nacked_while_running),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(7, response)
          expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
          expect_quorum_vote(evs[1], node, 'success')
          expect_quorum_succeeded(evs[2])
          expect_run_start(evs[3], node)
          expect_rehab(evs[4], node)
          expect_run_complete(evs[5], node, 'run_nacked_while_running')
          expect_job_complete(evs[6], "complete")
        end
      end
    end

  end

  context 'with a job with two nodes,' do
    let(:quorum) { 2 }
    let(:nodes) { ['DONKEY', 'FIONA'] }

    context "when the command succeeds on both" do
      before :each do
        start_new_clients(nodes)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(A,success),quorum_vote(B,success),quorum_succeeded,run_start(A),run_start(B),run_complete(A,success),run_complete(B,success),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
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
        start_new_clients(nodes)
        do_complete_job(job_to_run)
      end

      it "the events should be: start,quorum_vote(A,success),quorum_vote(B,success),quorum_succeeded,run_start(A),run_start(B),run_complete(A,failure),run_complete(B,failure),job_complete(complete)" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
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
        start_new_clients(nodes)
        # Do some ugly object hacking to get one client to behave differently
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
        get_job_feed(@id) do |response|
          evs = expect_valid_response(9, response)
          expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
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
        start_new_clients(nodes)
        # Do some ugly object hacking to get one client to behave differently
        donkey = @clients['DONKEY'][:client]
        jr = donkey.instance_variable_get('@job_runner')
        class <<jr
          def commit(job_id, command, opts)
            client.send_command(:nack_commit, job_id)
            return false
          end
        end
        job = job_to_run.merge({'quorum' => quorum})
        do_complete_job(job)
      end

      context "when the quorum is 100%" do
        it "the events should be: start,quorum_vote(A,success),quorum_vote(B,failure),job_complete(quorum_failed)" do
          get_job_feed(@id) do |response|
            evs = expect_valid_response(4, response)
            expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
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
          get_job_feed(@id) do |response|
            evs = expect_valid_response(7, response)
            expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
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

    context "when a buggy client sends an unrecognized message (e.g. nack_run) during a vote, after committing," do
      before :each do
        start_new_clients(nodes)
        # Do some ugly object hacking to get one client to behave differently
        donkey = @clients['DONKEY'][:client]
        jr = donkey.instance_variable_get('@job_runner')
        class <<jr
          def commit(job_id, command, opts)
            client.send_command(:ack_commit, job_id)
            client.send_command(:nack_run, job_id)
            true
          end
        end
        fiona = @clients['FIONA'][:client]
        jr = fiona.instance_variable_get('@job_runner')
        class <<jr
          def commit(job_id, command, opts)
            sleep 1
            client.send_command(:ack_commit, job_id)
          end
        end
        do_complete_job(job_to_run)
      end

      it "there should be a rehab event, and the job should fail" do
        get_job_feed(@id) do |response|
          evs = expect_valid_response(5, response)
          expect_start(evs[0], command, run_timeout, quorum, 2, admin_user.name)
          expect_quorum_vote(evs[1], 'DONKEY', 'success')
          expect_rehab(evs[2], 'DONKEY')
          expect_quorum_vote(evs[3], 'FIONA', 'success')
          expect_job_complete(evs[4], "quorum_failed")
        end
      end
    end
  end

  context "with a job that has been complete for a while," do
    let(:nodes) { ['DONKEY'] }
    before :each do
      start_new_clients(nodes)
      do_complete_job(job_to_run)
      sleep(SUMMARY_WAIT_TIME + 1)
    end

    it "the response to a feed request should just be the summary of the job" do
      get_job_feed(@id) do |response|
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
      start_new_clients([node])
      donkey = @clients['DONKEY'][:client]
      donkey.instance_variable_get('@whitelist').instance_variable_set('@whitelist', {command => command})
      do_complete_job(job_to_run)
    end

    it "the events should be cleanly encoded" do
      get_job_feed(@id) do |response|
        evs = expect_valid_response(6, response)
        expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
        expect_quorum_vote(evs[1], node, 'success')
        expect_quorum_succeeded(evs[2])
        expect_run_start(evs[3], node)
        expect_run_complete(evs[4], node, 'success')
        expect_job_complete(evs[5], "complete")
      end
    end
  end

  context "when reading an event stream," do
    let(:node) { 'DONKEY' }
    let(:nodes) { [node] }
    before :each do
      start_new_clients([node])
      @id = start_new_job(job_to_run)
      @stream = start_event_stream
    end

    it "the events become available as they happen" do
      evs = @stream.get_streaming_events
      validate_events(4, evs)
      expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
      expect_quorum_vote(evs[1], node, 'success')
      expect_quorum_succeeded(evs[2])
      expect_run_start(evs[3], node)
      @stream.close
      sleep 2
      evs = @stream.get_streaming_events
      validate_events(6, evs)
      expect_run_complete(evs[4], node, 'success')
      expect_job_complete(evs[5], "complete")
      @stream.complete.should == true
    end

    it "an additional stream also works" do
      another_stream = start_event_stream
      stream_evs = [@stream.get_streaming_events, another_stream.get_streaming_events]
      stream_evs.each do |evs|
        validate_events(4, evs)
        expect_start(evs[0], command, run_timeout, quorum, 1, admin_user.name)
        expect_quorum_vote(evs[1], node, 'success')
        expect_quorum_succeeded(evs[2])
        expect_run_start(evs[3], node)
      end
      sleep 2
      stream_evs = [@stream.get_streaming_events, another_stream.get_streaming_events]
      stream_evs.each do |evs|
        validate_events(6, evs)
        expect_run_complete(evs[4], node, 'success')
        expect_job_complete(evs[5], "complete")
      end
    end

    it "the stream can be resumed" do
      old_evs = @stream.get_streaming_events
      last_id = old_evs[3].id
      sleep 1
      another_stream = start_event_stream(last_id)
      new_evs = another_stream.get_streaming_events
      all_evs = @stream.get_streaming_events
      all_evs.should == old_evs+new_evs
    end

    it "an invalid LastEventId produces the entire stream" do
      another_stream = start_event_stream("bleah")
      evs = another_stream.get_streaming_events
      validate_events(4, evs)
    end
  end

  context "when reading an event stream with a slow-running client,", :slow => true do
    let(:command) { "sleep 21"}
    let(:run_timeout) { 25 }
    let(:node) { 'DONKEY' }
    let(:nodes) { [node] }
    before :each do
      start_new_clients([node])
      donkey = @clients[node][:client]
      donkey.instance_variable_get('@whitelist').instance_variable_set('@whitelist', {command => command})
      @id = start_new_job(job_to_run)
      @stream = start_event_stream(nil, 20) # receive timeout of 20 seconds
    end

    it "keepalives don't corrupt the stream" do
      # Ideally, this would also validate that keepalives are being sent at all (hence the timeout, above),
      # but I can't get the receive timeout to work -- no matter how long the connection is idle, there is never
      # a failure.  So this test passes even when there is no keep-alive data.
      sleep 21
      evs = @stream.get_streaming_events
      validate_events(6, evs)
    end
  end

  context "when creating a job with optional parameters," do
    let(:node) { 'DONKEY' }
    def param_job(user, dir, env, file)
      {'command' => 'ruby-opts', 'nodes' => [node], 'user' => user, 'dir' => dir, 'env' => env, 'file' => file}
    end
    before :each do
      start_new_clients([node])
    end

    it 'the job event should include the parameters (with "file_specified" instead of "file")' do
      env = {'ENV1' => 'myenv1', 'ENV2' => 'myenv2'}
      @id = start_new_job(param_job('root', '/tmp', env, 'raw:foo'))
      @stream = start_event_stream
      evs = @stream.get_streaming_events
      js = evs[0].json
      js['job_user'].should == 'root'
      js['dir'].should == '/tmp'
      js['env'].should == env
      js['file'].should == nil
      js['file_specified'].should == true
    end

    # As above, there is no org feed test here, because it can't easily be written in Ruby.
    # There is an Erlang test, however.
  end
end
