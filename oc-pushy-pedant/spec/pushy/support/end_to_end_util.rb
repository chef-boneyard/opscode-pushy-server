# @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
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

require 'pushy_client'
require 'timeout'

shared_context "end_to_end_util" do
  let (:sleep_time) { 0.2 }

  # A variety of timeouts are used to ensure that jobs have started,
  # nodes are available, etc.  These are set very conservatively.
  let (:client_start_timeout) { 5 }
  let (:job_start_timeout) { 30 }
  let (:job_status_timeout_default) { 30 }
  let (:node_availability_timeout) { 10 }
  let (:node_status_timeout) { 10 }
  let (:server_restart_timeout) { 45 } # increasing this makes failing tests take longer, but salvages some slow runs

  let (:client_creation_retries) { 5 }  # how many times to retry a client creation
  let (:client_creation_sleep) { 10 } # how long to wait between retries

  def echo_yahoo
    'sh ' + File.expand_path('../../support/echo_yahoo_to_tmp_pushytest', __FILE__)
  end

  # Command to use to tie up a node while we test the behavior of jobs
  # when some of their target nodes are busy (e.g., to test quorum
  # checking, job refusal, etc.)
  #
  # @note Depending on the load the test machine is experiencing, this
  # sleep may need to be lengthened.
  #
  # @note Whatever the command is, it must be in the push-job client's
  # whitelist
  def make_node_busy
    'sleep 5'
  end

  # Method to start up a new client that will be reaped when
  # the test finishes
  def start_new_clients(names, opts = {})
    @clients = {} if !@clients
    names.each do |name|
      raise "Client #{name} already created" if @clients[name]
      @clients[name] = {
        :states => []
      }
    end

    start_clients(names, opts)
  end

  def start_client(name, opts = {})
    start_clients([name], opts)
  end

  def start_clients(names, opts = {})
    names.each do |name|
      raise "Client #{name} already started" if @clients[name][:client]

      # Delete chef client if it exists
      delete(api_url("/clients/#{name}"), admin_user)

      require 'pp'

      # Create chef client and save key for pushy client
      #
      # Keygen can be slow, and fail
      retry_count = 1
      while (retry_count <= client_creation_retries)
        response = post(api_url("/clients"), superuser, :payload => {"name" => name})

        puts "Got a #{response.code} response to a POST to /clients for client #{name}: (try #{retry_count})"
        pp response
        # 500 happens when keygen is behind; generating a key can take almost a sec on a slow box
        break if response.code < 500
        sleep client_creation_sleep
        retry_count+=1
      end

      key = parse(response)["private_key"]
      #puts "Private Key for client #{name}:"
      #pp key

      @clients[name][:key_file] = file = Tempfile.new([name, '.pem'])
      key_path = file.path

      file.write(key)
      file.flush

      require 'rbconfig'
      ruby_exec = "BUNDLE_GEMFILE='' #{RbConfig.ruby} -e "

      # Create pushy client
      default_opts = {
        :chef_server_url => "#{Pedant.config[:chef_server]}/organizations/#{org}",
        :client_key      => key_path,
        :node_name       => name,
        :hostname        => name,
        :whitelist       => {
          echo_yahoo => echo_yahoo,
          'chef-client' => {
            :command_line => 'echo true',
            :lock => true
          },
          'ruby -e "ENV[\'CHEF_PUSH_NODE_NAME\'] == \'DONKEY\' ? exit(1) : exit(0)"' =>
             ruby_exec + '"ENV[\'CHEF_PUSH_NODE_NAME\'] == \'DONKEY\' ? exit(1) : exit(0)"',
          'ruby -e "exit 1"' => ruby_exec + '"exit 1"',
          'sleep 1' => 'sleep 1',
          'sleep 2' => 'sleep 2',
          'sleep 5' => 'sleep 5',
          'sleep 10' => 'sleep 10',
          'sleep 20' => 'sleep 20',
          'this_oughta_succeed' => 'echo true',
          'this_oughta_fail' => ruby_exec + '"exit 1"',
          'capture_test' => ruby_exec + %q!'puts "testout"; $stderr.puts "testerr"'!,
          'capture_test_fail' => ruby_exec + %q!'puts "testout"; $stderr.puts "testerr"; exit 1'!,
          'capture_test_empty' => '/bin/true',
          'capture_test_no_out' => ruby_exec + %q!'$stderr.puts "testerr"'!,
          'capture_test_no_err' => 'echo testout',
          'ruby-opts' => {
            :command_line => ruby_exec + %q!'$,="\n";p=Process;File.open(ENV["OUT"],"w"){|f|f.print p.uid,p.euid,Dir.getwd,ENV.to_a}'!
          },
          'ruby-junk' => {
              :command_line => ruby_exec + %q!'$,="\n";p=Process;File.open("/tmp/junkfile","w"){|f|f.print p.uid,p.euid,Dir.getwd,ENV.to_a} &> /tmp/junk-capture'!
          },
          'debug-env' => {
            :command_line => "(printenv; id; which ruby; echo #{ruby_exec}; #{ruby_exec}'puts :ruby_minus_e_ran') &> /tmp/debug-env-#{name}"
          }
        }
      }
      all_opts = default_opts.merge(opts)
      new_client = PushyClient.new(all_opts)
      @clients[name][:client] = new_client
      @clients[name][:client].start
    end

    names.each do |name|
      client =  @clients[name]

      # Register for state changes
      client[:states] << client[:client].job_state
      client[:client].on_job_state_change { |job_state| client[:states] << job_state }
    end

    begin
      Timeout::timeout(client_start_timeout) do
        while true
          offline_nodes = names.select { |name| !@clients[name][:client].online? }
          break if offline_nodes.size == 0
          sleep sleep_time
        end
      end
    rescue Timeout::Error
      raise "Clients never connected to the server: #{offline_nodes}"
    end

    # Wait for client to come out of rehab
    wait_for_node_to_come_out_of_rehab(*names)
  end

  def wait_for_node_status(up_down, *names)
    begin
      Timeout::timeout(node_status_timeout) do
        until names.all? { |name|
            get(api_url("pushy/node_states/#{name}"), admin_user) do |response|
              status = JSON.parse(response)['status']
              status == up_down
            end
          }
          sleep sleep_time
        end
      end
    rescue Timeout::Error
      node_states = {}
      names.each do |name|
        get(api_url("pushy/node_states/#{name}"), admin_user) do |response|
          status = JSON.parse(response)['status']
          node_states[name] = status
        end
      end
      raise "Not all nodes detected up by server!  #{node_states}"
    end
  end

  def wait_for_nodes_availabilty(availability, *names)
    begin
      Timeout::timeout(node_availability_timeout) do
        until names.all? { |name|
                response = get_rest("pushy/node_states/#{name}")
                response['availability'] == availability
              }
          sleep sleep_time
        end
      end
    rescue Timeout::Error
      nodes_in_rehab = {}
      names.each do |name|
        nodes_in_rehab[name] = get_rest("pushy/node_states/#{name}")
      end
      raise "Clients availability never never changed to #{availability}: #{nodes_in_rehab}"
    end
  end

  def wait_for_node_to_come_out_of_rehab(*names)
    wait_for_nodes_availabilty('available', *names)
  end

  def stop_client(name)
    client = @clients[name][:client]
    @clients[name][:client] = nil

    raise "Client #{name} already stopped" if !client

    # Trigger the stop in a different thread, since sometimes we're calling it
    # from a callback (on_job_state_change or send_command) on a client thread
    thread = Thread.new { client.stop }
    thread.join

    @clients[name][:key_file].close
  end

  def kill_client(name)
    stop_client(name)
  end

  def pushy_homedir
    "/opt/opscode-push-jobs-server/embedded/service/opscode-pushy-server"
  end

  def restart_server
    `#{pushy_homedir}/bin/opscode-pushy-server restart`
  end

  # When going through nginx as a lb, we'll get 502 HTML pages back from it
  # until the backend appears.  We wait until we get a parsable JSON object back
  def wait_for_server_restart
    begin
      Timeout::timeout(server_restart_timeout) do
        status = :not_ready
        while status != :ready do
          sleep(1)
          status = get(api_url("pushy/config/DONKEY"), admin_user) do |response|
            begin
              JSON.parse(response)
              :ready
            rescue
              :not_ready
            end
          end
        end
      end
    rescue Timeout::Error
      raise "Server Never Came Back"
    end
  end

  def get_node_state(node)
    get_rest("pushy/node_states/#{node}")
  end

  def wait_for_job_complete_or_fail(uri)
    wait_for_job_status(uri, ['complete', 'quorum_failed'])
  end

  def wait_for_job_complete(uri)
    wait_for_job_status(uri, 'complete')
  end

  def wait_for_job_status(uri, status, options = {})
    job = nil
    status_list = status.respond_to?(:member?) ? status : [status]
    begin
      Timeout::timeout(options[:timeout] || job_status_timeout_default) do
        begin
          sleep(sleep_time) if job
          job = get_job(uri)
        end until status_list.member?(job['status'])
      end
    rescue Timeout::Error
      raise "Job never reached status '#{status}': actual job reported as #{job}"
    end
    job
  end

  # Retrieves the job denoted by `uri`.  Removes the `id`,
  # `created_at`, and `updated_at` keys (as these are volatile and
  # change with each invocation of the tests) and sorts the lists of
  # nodes for each status.  Returns the JSON body as a Hash.
  #
  # If the HTTP GET to retrieve the job is not successful, an RSpec
  # matcher error will be raised, and your test will fail.
  #
  # @example Sample Return Value
  #  {
  #    "nodes"=>{
  #      "succeeded"=>["FARQUAD", "FIONA"],
  #      "nacked"=>["DONKEY"]
  #    },
  #    "command"=>"sh /do/this/thing --right-now",
  #    "status"=>"complete",
  #    "run_timeout"=>3600
  #  }
  #
  # @return [Hash]
  def get_job(uri)
    job = get(uri, admin_user) do |response|
      response.should look_like({:status => 200})
      JSON.parse(response)
    end
    job.delete('id')
    job.delete('created_at')
    job.delete('updated_at')
    job['nodes'].keys.each do |status|
      job['nodes'][status] = job['nodes'][status].sort
    end
    job
  end

  def start_echo_job_on_all_clients
    File.delete('/tmp/pushytest') if File.exist?('/tmp/pushytest')
    start_job_on_all_clients(echo_yahoo)
  end

  def start_job_on_all_clients(command)
    start_and_wait_for_job(command, @clients.keys)
  end

  def start_and_wait_for_job(command, node_names, options = {})
    @response = start_job(command, node_names, options)
    job_id = @response["uri"].split("/").last
    # Wait until all have started
    begin
      uncommitted_nodes = node_names # assume nothing is committed to start
      Timeout::timeout(job_start_timeout) do
        while true
          uncommitted_nodes = node_names.select do |name|
            !@clients[name][:states].any? do |state|
              state[:state] == :committed && state[:job_id] == job_id
            end
          end
          break if uncommitted_nodes.size == 0
          sleep(sleep_time)
        end
      end
    rescue Timeout::Error
      raise "Timeout of #{job_start_timeout} sec elapsed: Clients never committed to job, or job never started: #{uncommitted_nodes.map { |name| "#{name}: #{@clients[name][:states][-1]}" }}"
    end
  end

  def start_job(command, node_names, options = {} )
    payload = {'command' => command,
               'nodes' => node_names}.merge(options)
    post(api_url("pushy/jobs"), admin_user, :payload => payload) do |response|
      response.should look_like({:status => 201})
      JSON.parse(response)
    end
  end

  def echo_job_should_complete_on_all_clients
    job_should_complete(echo_yahoo, @clients.keys)
    IO.read('/tmp/pushytest').should == "YAHOO\n"*@clients.length
  end

  def job_should_complete_on_all_clients(command)
    job_should_complete(command, @clients.keys)
  end

  def job_should_complete(command, node_names, uri=@response['uri'])
    job = wait_for_job_complete(uri)
    job.should == {
      'command' => command,
      'run_timeout' => 3600,
      'nodes' => { 'succeeded' => node_names.sort },
      'status' => 'complete'
    }
  end

  def get_rest(uri)
    get(api_url(uri), admin_user) do |response|
      JSON.parse(response)
    end
  end

  def override_send_command(node_name, &block)
    old_send_command = @clients[node_name][:client].method(:send_command)
    @clients[node_name][:client].define_singleton_method(:send_command) do |message, job_id, params={}|
      block.call(old_send_command, message, job_id, params)
    end
  end

  def prep_tmp_path
    require 'tmpdir'
    path = Dir::Tmpname.create('pushypedant'){|p| p}
    ENV['OUT']=path
  end

  def read_tmp_path
    path = ENV['OUT']
    lines = IO.read(path).split($/)
    File.delete(path)
    lines
  end
end
