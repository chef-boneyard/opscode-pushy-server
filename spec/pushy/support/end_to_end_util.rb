require 'pushy-client'
require 'timeout'

shared_context "end_to_end_util" do
  SLEEP_TIME = 0.2

  def echo_yahoo
    'sh ' + File.expand_path('../../support/echo_yahoo_to_tmp_pushytest', __FILE__)
  end

  # Method to start up a new client that will be reaped when
  # the test finishes
  def start_new_clients(*names)
    @clients = {} if !@clients
    names.each do |name|
      raise "Client #{name} already created" if @clients[name]
      @clients[name] = {
        :states => []
      }
    end

    start_clients(*names)
  end

  def start_client(name)
    start_clients(name)
  end

  def start_clients(*names)
    names.each do |name|
      raise "Client #{name} already started" if @clients[name][:client]

      # Delete chef client if it exists
      delete(api_url("/clients/#{name}"), admin_user)

      # Create chef client and save key for pushy client
      response = post(api_url("/clients"), superuser, :payload => {"name" => name})
      key = parse(response)["private_key"]

      file = Tempfile.new([name, '.pem'])
      key_path = file.path

      file.write(key)
      file.close

      # Create pushy client
      new_client = PushyClient::App.new(
        :service_url_base        => "#{Pedant.config[:chef_server]}/organizations/#{org}",
        :client_private_key_path => key_path,
        :node_name               => name,
        :org_name                => org
      )
      @clients[name][:client] = new_client

      @clients[name][:thread] = Thread.new do
        new_client.start
      end
    end

    # Wait until client is registered with the server
    begin
      Timeout::timeout(5) do
        until names.all? { |name| @clients[name][:client].worker }
          sleep SLEEP_TIME
        end
      end
    rescue Timeout::Error
      raise "Clients never started: #{names.select { |name| !@clients[name][:client].worker }}"
    end

    names.each do |name|
      client =  @clients[name]

      # Register for state changes
      worker = client[:client].worker
      client[:states] << worker.job.state
      worker.on_state_change = Proc.new { |job| client[:states] << job.state }
    end

    begin
      Timeout::timeout(5) do
        until names.all? { |name| @clients[name][:client].worker.monitor.online? }
          sleep SLEEP_TIME
        end
      end
    rescue Timeout::Error
      raise "Clients never connected to the server: #{names.select { |name| !@clients[name][:client].worker.monitor.online? }}"
    end

    # Wait for client to come out of rehab
    wait_for_node_to_come_out_of_rehab(*names)
  end

  def wait_for_node_status(up_down, *names)
    begin
      Timeout::timeout(10) do
        until names.all? { |name|
            get(api_url("pushy/node_states/#{name}"), admin_user) do |response|
              status = JSON.parse(response)['status']
              status == up_down
            end
          }
          sleep SLEEP_TIME
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

  def wait_for_node_to_come_out_of_rehab(*names)
    begin
      Timeout::timeout(10) do
        until names.all? { |name|
                response = get_rest("pushy/node_states/#{name}")
                response['availability'] == 'available'
              }
          sleep SLEEP_TIME
        end
      end
    rescue Timeout::Error
      nodes_in_rehab = {}
      names.each do |name|
        nodes_in_rehab[name] = get_rest("pushy/node_states/#{name}")
      end
      raise "Clients never came out of rehab: #{nodes_in_rehab}"
    end
  end

  def stop_client(name)
    client = @clients[name][:client]
    @clients[name][:client] = nil

    raise "Client #{name} already stopped" if !client

    client.stop if client.worker
    @clients[name][:thread].kill
    @clients[name][:thread].join
  end

  def kill_client(name)
    client = @clients[name][:client]
    @clients[name][:client] = nil

    raise "Client #{name} already stopped" if !client

    # Do everything client.stop would do, without notifying anyone
    if client.worker
      client.worker.monitor.stop
      client.worker.timer.cancel
      client.worker.job.cancel
    end
    @clients[name][:thread].kill
    @clients[name][:thread].join
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
      Timeout::timeout(30) do
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


  def wait_for_job_complete(uri)
    wait_for_job_status(uri, 'complete')
  end

  def wait_for_job_status(uri, status, options = {})
    job = nil
    begin
      Timeout::timeout(options[:timeout] || 20) do
        begin
          sleep(SLEEP_TIME) if job
          job = get_job(uri)
        end until job['status'] == status
      end
    rescue Timeout::Error
      raise "Job never reached status '#{status}': actual job reported as #{job}"
    end
    job
  end

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
    # Wait until all have started
    begin
      Timeout::timeout(5) do
        until node_names.all? { |name| @clients[name][:states].include?(:ready) }
          sleep(SLEEP_TIME)
        end
      end
    rescue Timeout::Error
      raise "Clients never committed to job, or job never started: #{node_names.select { |name| !@clients[name][:states].include?(:ready) }.map { |name| "#{name}: #{@clients[name][:states][-1]}" }}"
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
end
