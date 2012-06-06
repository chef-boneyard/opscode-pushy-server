require 'time'
require 'pp'

module Pushy
  class Worker
    attr_reader :app, :monitor, :timer, :command

    attr_accessor :ctx
    attr_accessor :out_address
    attr_accessor :in_address
    attr_accessor :cmd_address
    attr_accessor :interval
    attr_accessor :offline_threshold
    attr_accessor :online_threshold
    attr_accessor :lifetime
    attr_accessor :client_private_key
    attr_accessor :server_public_key
    attr_accessor :node_name
    attr_accessor :state
    attr_accessor :subscriber
    attr_accessor :push_socket
    attr_accessor :cmd_socket

    def initialize(_app, options)
      @app = _app
      @state = "starting"

      @monitor = Pushy::Monitor.new(options)
      @ctx = EM::ZeroMQ::Context.new(1)
      @out_address = options[:out_address]
      @in_address = options[:in_address]
      @cmd_address = options[:cmd_address]
      @interval = options[:interval]
      @client_key_path = options[:client_key]
      @server_key_path = options[:server_key]

      @offline_threshold = options[:offline_threshold]
      @online_threshold = options[:online_threshold]
      @lifetime = options[:lifetime]

      @node_name = app.node_name
      @client_private_key = load_key(app.client_private_key_path)
      @server_public_key = OpenSSL::PKey::RSA.new(options[:server_public_key]) || load_key(options[:server_public_key_path])
    end

    def change_state(state)
      self.state = state
      send_heartbeat
    end

    def send_heartbeat
      return unless monitor.online?

      message = {:node => node_name,
        :org => "ORG",
        :state => state,
        :timestamp => Time.now.httpdate}

      send_signed_json(self.push_socket, message)
    end

    class << self
      def load!(app)
        from_hash(app, get_config_json(app))
      end

      def from_json(app, raw_json_config)
        from_hash(Yajl::Parser.parse(raw_json_config))
      end

      def from_hash(app, config)
        new app,
          :in_address        => config['push_jobs']['heartbeat']['in_addr'],
          :out_address       => config['push_jobs']['heartbeat']['out_addr'],
          :cmd_address       => config['push_jobs']['heartbeat']['command_addr'],
          :interval          => config['push_jobs']['heartbeat']['interval'],
          :offline_threshold => config['push_jobs']['heartbeat']['offline_threshold'],
          :online_threshold  => config['push_jobs']['heartbeat']['online_threshold'],
          :lifetime          => config['lifetime'],
          :server_public_key => config['public_key'],
      end

      def noauth_rest(app)
        @noauth_rest ||= begin
                           require 'chef/rest'
                           Chef::REST.new(app.service_url_base || DEFAULT_SERVICE_URL_BASE, false, false)
                         end
        @noauth_rest
      end

      def get_config_json(app)
        Pushy::Log.info "Worker: Fetching configuration ..."
        noauth_rest(app).get_rest("push_jobs/config", false)
      end
    end

    def start

      # TODO: Define hwm behavior for sockets below

      # Subscribe to heartbeat from the server
      Pushy::Log.info "Worker: Listening for server heartbeat at #{out_address}"
      self.subscriber = ctx.socket(ZMQ::SUB, Pushy::Handler::Heartbeat.new(monitor, self))
      self.subscriber.connect(out_address)
      self.subscriber.setsockopt(ZMQ::SUBSCRIBE, "")

      # Push heartbeat to server
      Pushy::Log.info "Worker: Broadcasting heartbeat at #{in_address}"
      self.push_socket = ctx.socket(ZMQ::PUSH)
      self.push_socket.setsockopt(ZMQ::LINGER, 0)
      self.push_socket.connect(in_address)

      # command socket for server
      Pushy::Log.info "Worker: Connecting to command channel at #{cmd_address}"
      # TODO
      # This needs to be set up to be able to handle bidirectional messages; right now this is Tx only
      # Probably need to set it up with a handler, like the subscriber socket above.
      self.cmd_socket = ctx.socket(ZMQ::DEALER, Pushy::Handler::Command.new(self))
      self.cmd_socket.setsockopt(ZMQ::LINGER, 0)
      self.cmd_socket.connect(cmd_address)

      monitor.start

      Pushy::Log.debug "Worker: Setting heartbeat at every #{interval} seconds"
      @timer = EM::PeriodicTimer.new(interval) do
        send_heartbeat
      end

      # TODO
      # This whole section is test code; I just wanted to send a message to the server to verify things work
      # We want to send a 'ready' message on startup, and whenever we lose the connection to the server or otherwise reconfigure
      @command = EM::PeriodicTimer.new(interval*5) do
          message = {:node => node_name,
          :client => (`hostname`).chomp,
          :org => "ORG",
          :type => "echo",
          :command => "ps aux",
          :timestamp => Time.now.httpdate
          }
        pp ["Sending message:", message]
        send_signed_json(cmd_socket, message)
      end

      change_state "idle"

    end

    def stop
      Pushy::Log.debug "Worker: Stopping ..."
      monitor.stop
      timer.cancel
      command.cancel
      Pushy::Log.debug "Worker: Stopped."
    end

    private

    def load_key(key_path)
      raw_key = IO.read(key_path).strip
      OpenSSL::PKey::RSA.new(raw_key)
    end

    def sign_checksum(json)
      checksum = Mixlib::Authentication::Digester.hash_string(json)
      Base64.encode64(client_private_key.private_encrypt(checksum)).chomp
    end

    def send_signed_json(socket, message)
      json = Yajl::Encoder.encode(message)
      sig = sign_checksum(json)
      auth = "VersionId:0.0.1;SignedChecksum:#{sig}"

      Pushy::Log.debug "Sending Message #{json}"
      
      socket.send_msg(auth, json)
    end

  end
end
