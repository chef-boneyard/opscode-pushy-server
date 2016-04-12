# -*- indent-level: 2;indent-tabs-mode: nil; fill-column: 92 -*-
#
# Author:: John Keiser (<jkeiser@opscode.com>)
# Author:: Douglas Triggs (<doug@opscode.com>)
#

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
require 'fileutils'

def is_backend
  !!Pedant.config[:running_from_backend]
end

#
# Some test envs have bad perms on their path; this hides it for us.
def fixup_cmd_err(cmd_err)
  cmd_err.gsub(/^.*warning: Insecure world writable dir.*\n/,"")
end

PRETEST_SLEEP = 0.1
def sleep_and_wait_for_available(names)
  puts "names: #{names}"
  wait_for_node_to_come_out_of_rehab(*names)
end

describe "end-to-end-test" do
  include_context "end_to_end_util"

  let(:status_url) { api_url("/_status").gsub(/organizations\/[^\/]*.\//, '/pushy') }

  before :all do
    if (Pedant::Config.pushy_client_debug)
      Chef::Log.level = :debug
    end
  end

  after :each do
    if @clients
      @clients.each do |client_name, client|
        stop_client(client_name) if @clients[client_name][:client]
      end
      @clients = nil
    end
  end

  context 'with one client' do
    before :each do
      start_new_clients(['DONKEY'])
      sleep_and_wait_for_available(['DONKEY'])
    end

    it "heartbeat should be received when starting up" do
      client = @clients['DONKEY'][:client]
      threshold = client.config['push_jobs']['heartbeat']['offline_threshold']
      heartbeater = client.instance_variable_get(:@heartbeater)
      heartbeater.instance_variable_set(:@online, false)
      sleep ((threshold + 1) * 10)  # need a little extra time, just in case
      client.online?.should == true
    end

    it 'node count should be 1' do
      get(status_url, admin_user) do |response|
        response.should look_like({
          :node_fsm_count => 1
        })
      end
    end

    context 'that is already running chef-client' do
      before :each do
        # create a lockfile to simulate a chef-client run
        lockfile_location = Chef::Config[:lockfile] || "#{Chef::Config[:file_cache_path]}/chef-client-running.pid"
        # Ensure the directory for the lockfile exists
        FileUtils.mkdir_p(File.expand_path('..', lockfile_location))
        @lockfile = File.open(lockfile_location, File::RDWR|File::CREAT, 0644)
        @lockfile.flock(File::LOCK_EX|File::LOCK_NB)
      end

      after :each do
        # release the lock
        @lockfile.flock(File::LOCK_UN)
        @lockfile.close
      end

      it 'should nack when asked to commit to another job'  do
        job = start_job('chef-client', %w{DONKEY})
        get_job(job['uri']).should == {
          'command' => 'chef-client',
          'run_timeout' => 3600,
          'nodes' => { 'nacked' => [ 'DONKEY' ] },
          'status' => 'quorum_failed'
        }
      end

    end

    context 'when running a job' do
      before(:each) do
        get_node_state("DONKEY")
        start_echo_job_on_all_clients
      end

      it 'is marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end

    context 'when running a job and a client goes down and quickly back up' do
      before :each do
        # Here we do a longer-running job since we want to capture it
        # in the running state.  We want to ensure that we catch it
        # there, regardless of system load.
        @job = start_job(make_node_busy, %w{DONKEY})
        wait_for_job_status(@job['uri'], 'running')
        stop_client('DONKEY')
        start_client('DONKEY')
      end

      it 'should be marked as unavailable immediatly' do
        get_job(@job['uri']).should == {
          'command' => make_node_busy,
          'run_timeout' => 3600,
          'nodes' => { 'crashed' => [ 'DONKEY' ] },
          'status' => 'complete'
        }
      end
    end

    context 'when running a failing job' do
      let(:command) {'ruby -e "exit 1"'}
      before(:each) do
        @job1 = start_job(command, %w{DONKEY})
      end

      it 'should be marked as failed' do
        wait_for_job_complete(@job1['uri'])
        get_job(@job1['uri']).should == {
          'command' => command,
          'run_timeout' => 3600,
          'nodes' => { 'failed' => [ 'DONKEY' ] },
          'status' => 'complete'
        }
      end
    end

    context 'when client shuts down' do
      before(:each) do
        stop_client('DONKEY')
        wait_for_node_status('offline', 'DONKEY')
      end

      it 'node count should be 0' do
        get(status_url, admin_user) do |response|
          response.should look_like({
            :node_fsm_count => 0
          })
        end
      end
    end

    context 'when running a long running job' do
      before :each do
        start_and_wait_for_job('sleep 10', [ 'DONKEY' ])
      end

      context 'when the client sends an unexpected message with a valid job_id' do
        before :each do
          client = @clients['DONKEY'][:client]
          job_id = @response["uri"].split("/").last
          client.send_command(:nack_commit, job_id)
        end

        it 'aborts the node and we can run another job on the node afterwards successfully' do
          job = wait_for_job_complete(@response["uri"])
          job['nodes'].should == { 'crashed' => [ 'DONKEY' ] }

          wait_for_node_to_come_out_of_rehab('DONKEY')

          start_echo_job_on_all_clients
          echo_job_should_complete_on_all_clients
        end
      end

      context 'when the client sends an unexpected message with a bad timestamp' do
        before :each do
          client = @clients['DONKEY'][:client]
          job_id = @response["uri"].split("/").last

          @expired_time = Time.now - (1000)
          PushyClient::ProtocolHandler::TimeSendWrapper.stub(:now).and_return(@expired_time, Time.now())

          client.send_command(:nack_commit, job_id)
        end

        it 'the message is ignored and the job completes successfully' do
          job = wait_for_job_complete(@response["uri"])
          job['nodes'].should == { 'succeeded' => [ 'DONKEY' ] }
        end
      end

      context 'when the client sends an unexpected message with a old, but good timestamp' do
        before :each do
          client = @clients['DONKEY'][:client]
          job_id = @response["uri"].split("/").last

          @expired_time = Time.now - (100) # assumes timeout is 500 s.
          PushyClient::ProtocolHandler::TimeSendWrapper.stub(:now).and_return(@expired_time, Time.now())

          client.send_command(:nack_commit, job_id)
        end

        it 'aborts the node and we can run another job on the node afterwards successfully' do
          job = wait_for_job_complete(@response["uri"])
          job['nodes'].should == { 'crashed' => [ 'DONKEY' ] }

          wait_for_node_to_come_out_of_rehab('DONKEY')

          start_echo_job_on_all_clients
          echo_job_should_complete_on_all_clients
        end
      end


      context 'when the client sends an unexpected message with an invalid job_id' do
        before :each do
          client = @clients['DONKEY'][:client]
          client.send_command(:nack_commit, 'a')
        end

        it 'aborts the node and we can run another job on the node afterwards successfully' do
          job = wait_for_job_complete(@response["uri"])
          job['nodes'].should == { 'crashed' => [ 'DONKEY' ] }

          wait_for_node_to_come_out_of_rehab('DONKEY')

          start_echo_job_on_all_clients
          echo_job_should_complete_on_all_clients
        end
      end

    end

    context 'which sends aborted with a nonexistent job id' do
      before(:each) do
        @clients['DONKEY'][:client].should_not_receive(:abort)
        @clients['DONKEY'][:client].send_command(:aborted, 'a')
      end

      it 'does not go into rehab (receives no abort)' do
        sleep(1)
      end
    end

    context 'when running a job that completes before its timeout' do
      before(:each) do
        File.delete('/tmp/pushytest') if File.exist?('/tmp/pushytest')
        start_and_wait_for_job(echo_yahoo, [ 'DONKEY' ], :run_timeout => 2)
      end

      it 'completes' do
        job = wait_for_job_complete(@response['uri'])
        job.should == {
          'command' => echo_yahoo,
          'run_timeout' => 2,
          'nodes' => { 'succeeded' => [ 'DONKEY' ] },
          'status' => 'complete'
        }
        IO.read('/tmp/pushytest').should == "YAHOO\n"*@clients.length
      end
    end

    context 'when running a job that lasts longer than its timeout' do
      before(:each) do
        File.delete('/tmp/pushytest') if File.exist?('/tmp/pushytest')
        start_and_wait_for_job('sleep 2', [ 'DONKEY' ], :run_timeout => 1)
      end

      it 'times out and aborts' do
        job = wait_for_job_status(@response['uri'], 'timed_out')
        job.should == {
          'command' => 'sleep 2',
          'run_timeout' => 1,
          'nodes' => { 'timed_out' => [ 'DONKEY' ] },
          'status' => 'timed_out'
        }
        sleep(1.2)
        expect(File.exist?('/tmp/pushytest')).to be_falsey
      end
    end

    context 'that forgets to send the ack_commit message', :slow do
      before :each do
        override_send_command('DONKEY') do |real_send_command, message, job_id, params|
          real_send_command.call(message, job_id, params) unless message == :ack_commit
        end
      end

      it 'job times out and fails to start' do
        response = start_job(echo_yahoo, ['DONKEY'])
        get(api_url("pushy/node_states/DONKEY"), admin_user) do |r|
          r.should look_like({
                                      :status => 200,
                                      :body => {
                                        'status' => 'online'
                                      }})
        end
        # While we're waiting, let's verify that the node shows as "new".  This
        # is the only test where we're guaranteed to get this response--most
        # tests will vote immediately.
        get(response['uri'], admin_user) do |r|
          r.should look_like({
            :status => 200,
            :body => {
              'nodes' => { 'new' => ['DONKEY'] }
            }
          })
        end
        job = wait_for_job_status(response['uri'], 'quorum_failed', :timeout => 65)
        job['nodes'].should == { 'unavailable' => [ 'DONKEY' ] }
        # This verifies our assumption that this was caused by the TIMEOUT rather
        # than the node being detected as down
        get(api_url("pushy/node_states/DONKEY"), admin_user) do |r|
          r.should look_like({
                                      :status => 200,
                                      :body => {
                                        'status' => 'online'
                                      }})
        end
      end
    end

    context 'that dies before running a job' do
      before :each do
        stop_client('DONKEY')
        wait_for_node_status('offline', 'DONKEY')
      end

      it 'job immediately fails to start' do
        response = start_job(echo_yahoo, ['DONKEY'])

        # TODO check immediacy!  This could erroneously succeed on timing out.
        job = wait_for_job_status(response['uri'], 'quorum_failed')
        job['nodes'].should == { 'unavailable' => [ 'DONKEY' ] }
      end
    end

    context 'that dies just before running the job, but has not yet been detected as down' do
      before :each do
        stop_client('DONKEY')
      end

      it 'job fails to start when down is detected' do
        response = start_job(echo_yahoo, ['DONKEY'])
        get(api_url("pushy/node_states/DONKEY"), admin_user) do |r|
          r.should look_like({
                                      :status => 200,
                                      :body => {
                                        'status' => 'online'
                                      }})
        end
        # TODO we should ensure that this happened due to down detection, not
        # timeout.  Fine for now, because there is no timeout :)
        job = wait_for_job_status(response['uri'], 'quorum_failed')
        job['nodes'].should == { 'unavailable' => [ 'DONKEY' ] }
      end
    end

    context 'when the client crashes after reporting "ready" but before running the command' do
      before :each do
        override_send_command('DONKEY') do |real_send_command, message, job_id, params|
          real_send_command.call(message, job_id, params)
          if message == :ack_commit
            kill_client('DONKEY')
          end
        end
      end

      it 'job marks node as crashed when down is detected' do
        response = start_job(echo_yahoo, ['DONKEY'])
        get(api_url("pushy/node_states/DONKEY"), admin_user) do |r|
          r.should look_like({
                                      :status => 200,
                                      :body => {
                                        'status' => 'online'
                                      }})
        end
        # TODO we should ensure that this happened due to down detection, not
        # timeout.  Fine for now, because there is no timeout :)
        job = wait_for_job_status(response['uri'], 'complete')
        job['nodes'].should == { 'crashed' => [ 'DONKEY' ] }
      end
    end

    context 'when the client crashes after running but before completing the command' do
      before :each do
        # Set it up so the client will crash as soon as it changes to "voting"
        client = @clients['DONKEY'][:client]
        client.on_job_state_change { |state| kill_client('DONKEY') if state[:state] == :running }
      end

      it 'job marks node as crashed when down is detected' do
        response = start_job(echo_yahoo, ['DONKEY'])
        get(api_url("pushy/node_states/DONKEY"), admin_user) do |r|
          r.should look_like({
                                      :status => 200,
                                      :body => {
                                        'status' => 'online'
                                      }})
        end
        # TODO we should ensure that this happened due to down detection, not
        # timeout.  Fine for now, because there is no timeout :)
        job = wait_for_job_status(response['uri'], 'complete')
        job['nodes'].should == { 'crashed' => [ 'DONKEY' ] }
      end
    end

    context 'whitelist tests' do
      it 'trying to run a command that is not on the whitelist fails' do
        job = start_job('echo nevereverever', ['DONKEY'])
        job = wait_for_job_status(job['uri'], 'quorum_failed')
        job.should == {
          'command' => 'echo nevereverever',
          'run_timeout' => 3600,
          'nodes' => {
            'nacked' => [ 'DONKEY' ]
          },
          'status' => 'quorum_failed'
        }
      end

      it 'running a command with a shortened whitelist name that should succeed, it succeeds' do
        job = start_job('this_oughta_succeed', ['DONKEY'])
        job = wait_for_job_status(job['uri'], 'complete')
        job.should == {
          'command' => 'this_oughta_succeed',
          'run_timeout' => 3600,
          'nodes' => {
            'succeeded' => [ 'DONKEY' ]
          },
          'status' => 'complete'
        }
      end

      it 'running a command with a shortened whitelist name that should fail, it fails' do
        job = start_job('this_oughta_fail', ['DONKEY'])
        job = wait_for_job_status(job['uri'], 'complete')
        job.should == {
          'command' => 'this_oughta_fail',
          'run_timeout' => 3600,
          'nodes' => {
            'failed' => [ 'DONKEY' ]
          },
          'status' => 'complete'
        }
      end
    end

    context 'optional param tests' do
      it 'passing in a user, directory, env that should succeed, succeeds' do
        prep_tmp_path
        dir = "/tmp"
        job_env = {'E1' => 'e1', 'E2' => 'e2'}
        job = start_job('ruby-opts', ['DONKEY'], {'user' => 'daemon', 'dir' => dir, 'env' => job_env})
        wait_for_job_complete(job['uri'])
        # This would be better written with Process::UID.from_name, but that's Ruby 2.0
        require 'etc'
        uids = Etc.getpwnam('daemon').uid.to_s
        lines = read_tmp_path
        lines[0].should == uids
        lines[1].should == uids
        lines[2].should == dir
        envs = lines[3]
        envs['"E1", "e1"'].should_not == nil
        envs['"E2", "e2"'].should_not == nil
      end

      it 'passing in a bad user, fails in the commit' do
        prep_tmp_path
        job = start_job('ruby-opts', ['DONKEY'], {'user' => 'nonexistentuser'})
        wait_for_job_status(job['uri'], 'quorum_failed')
      end

      it 'passing in a bad directory, fails in the commit' do
        prep_tmp_path
        job = start_job('ruby-opts', ['DONKEY'], {'dir' => '/nonexistentdir'})
        wait_for_job_status(job['uri'], 'quorum_failed')
      end

      it 'CHEF_PUSH_JOB_ID is set to the job_id' do
        prep_tmp_path
        job = start_job('ruby-opts', ['DONKEY'])
        wait_for_job_complete(job['uri'])
        job_id = job["uri"].split("/").last
        envs = read_tmp_path[3]
        env = Hash[eval envs]
        env['CHEF_PUSH_JOB_ID'].should == job_id
      end

      it 'passing in a file as base64 works' do
        prep_tmp_path
        filestr = 'test'
        file64 = 'base64:' + Base64.strict_encode64(filestr)
        job = start_job('ruby-opts', ['DONKEY'], {'file' => file64})
        wait_for_job_complete(job['uri'])
        envs = read_tmp_path[3]
        env = Hash[eval envs]
        path = env['CHEF_PUSH_JOB_FILE']
        File.read(path).should == filestr
      end

      it 'passing in a file as raw works' do
        prep_tmp_path
        filestr = 'test'
        file_asc = 'raw:' + filestr
        job = start_job('ruby-opts', ['DONKEY'], {'file' => file_asc})
        wait_for_job_complete(job['uri'])
        envs = read_tmp_path[3]
        env = Hash[eval envs]
        path = env['CHEF_PUSH_JOB_FILE']
        File.read(path).should == filestr
      end

      it 'default file directory should contain "pushy/"' do
        prep_tmp_path
        filestr = 'test'
        file_asc = 'raw:' + filestr
        job = start_job('ruby-opts', ['DONKEY'], {'file' => file_asc})
        wait_for_job_complete(job['uri'])
        envs = read_tmp_path[3]
        env = Hash[eval envs]
        path = env['CHEF_PUSH_JOB_FILE']
        path.should match(/pushy\//)
      end

      it 'file directory can be configured' do
        prep_tmp_path
        new_file_dir = "/tmp/pushy2"
        stop_client('DONKEY')
        start_client('DONKEY', {:file_dir => new_file_dir})
        filestr = 'raw:test'
        job = start_job('ruby-opts', ['DONKEY'], {'file' => filestr})
        wait_for_job_complete(job['uri'])
        envs = read_tmp_path[3]
        env = Hash[eval envs]
        path = env['CHEF_PUSH_JOB_FILE']
        path.should start_with new_file_dir
      end

      it 'if post is too big (> 1M), server returns a 413' do
        prep_tmp_path
        file_asc = 'raw:' + ('X' * 1000000)
        payload = {'command' => 'ruby-opts',
                   'nodes' => ['DONKEY'],
                   'file' => file_asc}
        post(api_url("pushy/jobs"), admin_user, :payload => payload) do |response|
          response.should look_like({:status => 413})
        end
      end

      it 'if file is too big (> 100K), server returns a 400' do
        prep_tmp_path
        file_asc = 'raw:' + ('X' * 100001)
        payload = {'command' => 'ruby-opts',
                   'nodes' => ['DONKEY'],
                   'file' => file_asc}
        post(api_url("pushy/jobs"), admin_user, :payload => payload) do |response|
          response.should look_like({:status => 400})
        end
      end

      context "when getting the summary" do
        before :each do
          prep_tmp_path
        end

        it 'the summary includes the param information' do
          job_env = {'E1' => 'e1', 'E2' => 'e2'}
          job = start_job('ruby-opts', ['DONKEY'], {'user' => 'daemon', 'dir' => '/tmp', 'env' => job_env})
          uri = job['uri']
          wait_for_job_complete(uri)
          summary = get_job(uri)
          summary['user'].should == 'daemon'
          summary['dir'].should == '/tmp'
          summary['env'].should == job_env
          summary['file_specified'].should == nil
        end

        it 'if a file was provided, and the include_file is not specified, return "file_specified=true"' do
          job = start_job('ruby-opts', ['DONKEY'], {'file' => 'raw:foo'})
          uri = job['uri']
          wait_for_job_complete(uri)
          summary = get_job(uri)
          summary['user'].should == nil
          summary['dir'].should == nil
          summary['env'].should == nil
          summary['file_specified'].should == true
        end

        it 'if a file was provided, and the include_file is specified, return "file=<file>"' do
          file = 'raw:foo'
          job = start_job('ruby-opts', ['DONKEY'], {'file' => file})
          uri = job['uri']
          wait_for_job_complete(uri)
          summary = get_job(uri + '?include_file=true')
          summary['file_specified'].should == nil
          summary['file'].should == file
        end
      end
    end

    context 'capture tests' do
      before :each do
        sleep 1
      end

      it 'if capture is not specified, no data will be sent in result' do
        send_params = nil
        override_send_command('DONKEY') do |real_send_command, message, job_id, params|
          send_params = params
          real_send_command.call(message, job_id, params)
        end
        job = start_job('capture_test', ['DONKEY'])
        uri = job['uri']
        wait_for_job_complete(uri)
        send_params.should == {}
      end

      it 'if capture is specified, but output is empty, empty strings will be sent in result' do
        send_params = nil
        override_send_command('DONKEY') do |real_send_command, message, job_id, params|
          send_params = params
          real_send_command.call(message, job_id, params)
        end
        job = start_job('capture_test_empty', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        send_params.should == {:stdout => '', :stderr => ''}
      end

      it 'if capture is specified in a succeeding cmd, the stdout and stderr will be available via REST' do
        job = start_job('capture_test', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == "testout\n"
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        fixup_cmd_err(cmd_err).should == "testerr\n"
      end

      it 'if capture is specified in a failing cmd, the stdout and stderr will be available via REST' do
        job = start_job('capture_test_fail', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == "testout\n"
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        fixup_cmd_err(cmd_err).should == "testerr\n"
      end

      it 'if capture is not specified in a succeeding cmd, the stdout and stderr will return 404' do
        job = start_job('capture_test', ['DONKEY'])
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 404})
        end
        get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 404})
        end
      end

      it 'if capture is specified in a cmd, and the output is empty, the stdout and stderr will return the right strings' do
        job = start_job('capture_test_empty', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == ''
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_err.should == ''
      end

      it 'if capture is specified in a succeeding cmd with no err, the stdout and stderr will be available, and stderr will be empty' do
        job = start_job('capture_test_no_err', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == "testout\n"
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_err.should == ''
      end

      it 'if capture is specified in a succeeding cmd with no out, the stdout and stderr will be available, and stdout will be empty' do
        job = start_job('capture_test_no_out', ['DONKEY'], {'capture_output' => true})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        fixup_cmd_err(cmd_err).should == "testerr\n"
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == ''
      end

      it 'if capture is specified along with a user, things still work' do
        job = start_job('capture_test', ['DONKEY'], {'capture_output' => true,
                                                     'user' => 'daemon'})
        uri = job['uri']
        wait_for_job_complete(uri)
        opts = { :headers => {'Accept' => 'application/octet-stream'} }
        cmd_out = get(uri + "/output/DONKEY/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == "testout\n"
        cmd_err = get(uri + "/output/DONKEY/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        fixup_cmd_err(cmd_err).should == "testerr\n"
      end
    end
  end

  context 'with a client that is killed and comes back up quickly' do
    before :each do
      start_new_clients(['DONKEY'])
      sleep_and_wait_for_available(['DONKEY'])
      kill_client('DONKEY')
      start_client('DONKEY')
    end

    context 'when running a job' do
      before(:each) do
        start_echo_job_on_all_clients
      end

      it 'is marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end
  end

  context 'with a dead client that comes back down after a while' do
    before :each do
      start_new_clients(['DONKEY'])
      sleep_and_wait_for_available(['DONKEY'])
      kill_client('DONKEY')
      wait_for_node_status('offline', 'DONKEY')
      # Start that sucker back up
      start_client('DONKEY')
    end

    context 'when running a job' do
      before(:each) do
        start_echo_job_on_all_clients
      end

      it 'is marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end
  end

  context 'with a client that goes down and back up quickly' do
    before :each do
      start_new_clients(['DONKEY'])
      sleep_and_wait_for_available(['DONKEY'])
      stop_client('DONKEY')
      start_client('DONKEY')
    end

    context 'when running a job' do
      before(:each) do
        start_echo_job_on_all_clients
      end

      it 'is marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end
  end

  context 'with a client that goes down and back up a while later' do
    before :each do
      start_new_clients(['DONKEY'])
      stop_client('DONKEY')
      wait_for_node_status('offline', 'DONKEY')
      start_client('DONKEY')
    end

    context 'when running a job' do
      before(:each) do
        start_echo_job_on_all_clients
      end

      it 'is marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end
  end

  context 'with three clients' do
    before :each do
      clients = ['DONKEY', 'FARQUAD', 'FIONA']
      start_new_clients(clients)
      sleep_and_wait_for_available(clients)
    end

    it 'node count should be 3' do
      get(status_url, admin_user) do |response|
        response.should look_like({
          :node_fsm_count => 3
        })
      end
    end

    context 'when a client shuts down' do
      before(:each) do
        stop_client('DONKEY')
        wait_for_node_status('offline', 'DONKEY')
      end

      it 'node count should be 2' do
        get(status_url, admin_user) do |response|
          response.should look_like({
            :node_fsm_count => 2
          })
        end
      end
    end

    context 'when all clients shut down' do
      before(:each) do
        stop_client('DONKEY')
        stop_client('FARQUAD')
        stop_client('FIONA')
        wait_for_node_status('offline', 'DONKEY', 'FARQUAD', 'FIONA')
      end

      it 'node count should be 0' do
        get(status_url, admin_user) do |response|
          response.should look_like({
            :node_fsm_count => 0
          })
        end
      end
    end

    context 'when running a job' do
      before(:each) do
        start_echo_job_on_all_clients
      end

      it 'the job and node statuses are marked complete' do
        echo_job_should_complete_on_all_clients
      end
    end

    context 'when running a job that DONKEY fails but FARQUAD and FIONA succeed' do
      before(:each) do
        @command = "ruby -e \"ENV['CHEF_PUSH_NODE_NAME'] == 'DONKEY' ? exit(1) : exit(0)\""
        @job1 = start_job(@command, %w{DONKEY FARQUAD FIONA})
      end

      it 'should report success on FARQUAD and FIONA and failed on DONKEY' do
        wait_for_job_complete(@job1['uri'])
        get_job(@job1['uri']).should == {
          'command' => @command,
          'run_timeout' => 3600,
          'nodes' => {
            'succeeded' => [ 'FARQUAD', 'FIONA' ],
            'failed' => [ 'DONKEY' ]
          },
          'status' => 'complete'
        }
      end
    end

    context 'when running a job on FARQUAD and FIONA' do
      before(:each) do
        @job1 = start_job('sleep 2', %w{FARQUAD FIONA})
      end

      context 'and we start a job on DONKEY, FARQUAD and FIONA with a quorum of 2' do
        before(:each) do
          @job2 = start_job(echo_yahoo, %w{DONKEY FARQUAD FIONA}, {'quorum' => 2})
        end

        it 'should fail with reason quorum_failed'  do
          wait_for_job_status(@job2['uri'], 'quorum_failed')
          get_job(@job2['uri']).should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'FARQUAD', 'FIONA' ],
              'was_ready' => [ 'DONKEY' ]
            },
            'status' => 'quorum_failed'
          }
          job_should_complete('sleep 2', %w{FARQUAD FIONA}, @job1['uri'])
        end
      end
    end

    context 'when running one job on DONKEY' do
      before(:each) do
        File.delete('/tmp/pushytest') if File.exist?('/tmp/pushytest')
        @job1 = start_job(make_node_busy, ['DONKEY'])
      end

      context 'and simultaneous job on FARQUAD and FIONA' do
        before(:each) do
          @job2 = start_job(echo_yahoo, ['FARQUAD', 'FIONA'])
        end

        it 'both jobs complete successfully' do
          job_should_complete(make_node_busy, %w{DONKEY}, @job1['uri'])
          job_should_complete(echo_yahoo, %w{FARQUAD FIONA}, @job2['uri'])
          IO.read('/tmp/pushytest').should == "YAHOO\n"*2
        end
      end

      context 'and we start a job on DONKEY, FARQUAD, and FIONA with a quorum of 2' do
        before(:each) do
          @job3 = start_job(echo_yahoo, %w{DONKEY FARQUAD FIONA}, {'quorum' => 2})
          wait_for_job_complete(@job3['uri'])
        end

        it 'should complete on FARQUAD and FIONA' do
          get_job(@job3['uri']).should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'DONKEY' ],
              'succeeded' => [ 'FARQUAD', 'FIONA' ]
            },
            'status' => 'complete'
          }
          job_should_complete(make_node_busy, %w{DONKEY}, @job1['uri'])
        end
      end

      context 'and we start a job on DONKEY and FIONA with a quorum of 2' do
        before(:each) do
          @job4 = start_job(echo_yahoo, %w{DONKEY FIONA}, {'quorum' => 2})
        end

        it 'should fail with reason quorum_failed' do
          wait_for_job_status(@job4['uri'], 'quorum_failed')
          get_job(@job4['uri']).should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'DONKEY' ],
              'was_ready' => [ 'FIONA' ]
            },
            'status' => 'quorum_failed'
          }
          job_should_complete(make_node_busy, %w{DONKEY}, @job1['uri'])
        end
      end

    end

    context 'with one tied up in a long-running job' do
      before(:each) do
        @job1 = start_job(make_node_busy, [ 'DONKEY' ])
      end

      context 'and we try to run a new job on all three nodes' do
        before(:each) do
          @nack_job = start_job(echo_yahoo, [ 'DONKEY', 'FARQUAD', 'FIONA' ])
        end

        it 'nacks the one and fails to run, and old job still completes' do
          wait_for_job_status(@nack_job['uri'], 'quorum_failed')

          nack_job = get_job(@nack_job['uri'])
          nack_job.should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'DONKEY' ],
              'was_ready' => [ 'FARQUAD', 'FIONA' ]
            },
            'status' => 'quorum_failed'
          }
          job_should_complete(make_node_busy, %w{DONKEY}, @job1['uri'])
        end
      end

      context 'and we try to run two other jobs on the node' do
        before(:each) do
          @nack_job = start_job(echo_yahoo, ['DONKEY'])
          @nack_job_2 = start_job(echo_yahoo, ['DONKEY'])
        end

        it 'nacks them both, and old job still completes' do

          nack_job = get_job(@nack_job['uri'])
          nack_job.should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'DONKEY' ]
            },
            'status' => 'quorum_failed'
          }

          nack_job_2 = get_job(@nack_job_2['uri'])
          nack_job_2.should == {
            'command' => echo_yahoo,
            'run_timeout' => 3600,
            'nodes' => {
              'nacked' => [ 'DONKEY' ]
            },
            'status' => 'quorum_failed'
          }
          job_should_complete(make_node_busy, [ 'DONKEY' ], @job1['uri'])
        end
      end
    end

    it 'capture works' do
      nodes = ['DONKEY', 'FARQUAD', 'FIONA']
      job = start_job('capture_test', nodes, {'capture_output' => true})
      uri = job['uri']
      wait_for_job_complete(uri)
      opts = { :headers => {'Accept' => 'application/octet-stream'} }
      nodes.each do |node|
        cmd_out = get(uri + "/output/#{node}/stdout", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        cmd_out.should == "testout\n"
        cmd_err = get(uri + "/output/#{node}/stderr", admin_user, opts) do |response|
          response.should look_like({:status => 200})
          response
        end
        fixup_cmd_err(cmd_err).should == "testerr\n"
      end
    end
  end

  context 'when one client is running a long running job' do
    before :each do
      start_new_clients(['DONKEY'])
      sleep_and_wait_for_available(['DONKEY'])
      @long_job = start_job('sleep 5', [ 'DONKEY' ])
    end

    context 'and the client reconfigures' do
      before :each do
        wait_for_job_status(@long_job['uri'], 'running')
        @clients['DONKEY'][:client].reconfigure
      end

      it 'the job still completes' do
        job_should_complete('sleep 5', [ 'DONKEY' ], @long_job['uri'])
      end
    end
  end

  # This was moved to the bottom because it seems to be adversely affecting other tests
  # TODO Figure out why this breaks other tests and fix it
  context 'when one client is running a long running job' do
    before :each do
      start_new_clients(['DONKEY'])
      @long_job = start_job('sleep 20', [ 'DONKEY' ])
    end

    context 'and the server goes down and comes back up', :skip => !is_backend do
      # This should only run on a maching that runs backend services
      # because we currently shut down the pushy server by shelling
      # out; this doesn't work on frontend machines because there's no
      # service to shell out to.
      before :each do
        restart_server
        wait_for_server_restart
        wait_for_node_status("online", "DONKEY")
        wait_for_node_to_come_out_of_rehab("DONKEY")
      end

      it 'the client should abort and then be able to run another job, and the job should be set to crashed' do
        start_echo_job_on_all_clients
        echo_job_should_complete_on_all_clients
        wait_for_job_status(@long_job['uri'], 'crashed')
      end
    end
  end

  context 'bad input' do
    it '404s when retrieving a nonexistent job' do
      get(api_url('pushy/jobs/abcdefabcdef807f32d9572f8aafbd03'), admin_user) do |response|
        response.should look_like({:status=>404})
        # TODO return JSON error body
      end
    end
  end
end
