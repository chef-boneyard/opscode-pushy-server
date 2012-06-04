require 'time'
require 'pp'

module Pushy
  class Client
    attr_accessor :monitor
    attr_accessor :ctx
    attr_accessor :out_address
    attr_accessor :in_address
    attr_accessor :interval
    attr_accessor :offline_threshold
    attr_accessor :online_threshold
    attr_accessor :lifetime
    attr_accessor :client_private_key
    attr_accessor :server_public_key
    attr_accessor :node_name

    def initialize(options)
      @monitor = Pushy::Monitor.new(options)
      @ctx = EM::ZeroMQ::Context.new(1)
      @out_address = options[:out_address]
      @in_address = options[:in_address]
      @interval = options[:interval]
      @client_key_path = options[:client_key]
      @server_key_path = options[:server_key]
      @node_name = self.class.node_name

      @offline_threshold = options[:offline_threshold]
      @online_threshold = options[:online_threshold]
      @lifetime = options[:lifetime]

      @client_private_key = load_key(self.class.client_private_key_path)
      @server_public_key = OpenSSL::PKey::RSA.new(options[:server_public_key]) || load_key(options[:server_public_key_path])
    end

    class << self
      DEFAULT_SERVICE_URL_BASE = "localhost:10003/organization/clownco"
      attr_accessor :service_url_base
      attr_accessor :client_private_key_path
      attr_accessor :node_name

      def load!
        from_hash(get_config_json)
      end

      def reload!(old_client)
        old_client.stop if old_client
        load!.start
      end

      def from_json(raw_json_config)
        from_hash(Yajl::Parser.parse(raw_json_config))
      end

      def from_hash(config)
        new :in_address      => config['push_jobs']['heartbeat']['in_addr'],
            :out_address       => config['push_jobs']['heartbeat']['out_addr'],
            :interval          => config['push_jobs']['heartbeat']['interval'],
            :offline_threshold => config['push_jobs']['heartbeat']['offline_threshold'],
            :online_threshold  => config['push_jobs']['heartbeat']['online_threshold'],
            :lifetime          => config['lifetime'],
            :server_public_key => config['public_key'],
      end

      def noauth_rest
        @noauth_rest ||= begin
                           require 'chef/rest'
                           Chef::REST.new(self.service_url_base || DEFAULT_SERVICE_URL_BASE, false, false)
                         end
        @noauth_rest
      end

      def get_config_json
        noauth_rest.get_rest("push_jobs/config", false)
      end
    end

    def start
      Pushy::Log.info "Listening at #{out_address}"

      EM.run do

        # Subscribe to heartbeat from the server
        subscriber = ctx.socket(ZMQ::SUB, Pushy::Handler.new(monitor, self))
        subscriber.connect(out_address)
        subscriber.setsockopt(ZMQ::SUBSCRIBE, "")

        # Push heartbeat to server
        push_socket = ctx.socket(ZMQ::PUSH)
        push_socket.setsockopt(ZMQ::LINGER, 0)
        push_socket.connect(in_address)

        monitor.start

        seq = 0

        EM::PeriodicTimer.new(interval) do
          if monitor.online?

            json = Yajl::Encoder.encode({:node => node_name,
                                         :client => (`hostname`).chomp,
                                         :org => "ORG",
                                         :sequence => seq,
                                         :timestamp => Time.now.httpdate})

            auth = "VersionId:0.0.1;SignedChecksum:#{sign_checksum(json)}"

            Pushy::Log.debug "Sending Message #{json}"

            push_socket.send_msg(auth, json)

            seq += 1
          end
        end

      end

    end

    def stop
      monitor.stop
      EM.stop_event_loop
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

  end
end
