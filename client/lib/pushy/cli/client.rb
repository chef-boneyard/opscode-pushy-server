module Pushy
  module CLI
    class Client

      include Mixlib::CLI

      option :config_file,
        :short => "-c CONFIG",
        :long => "--config CONFIG",
        :default => "config.rb",
        :description => "The configuration file to use"

      option :node_name,
        :short => "-n NODENAME",
        :long => "--node-name NODENAME",
        :default => (`hostname`).chomp,
        :description => "The Node name"

      option :offline_threshold,
        :long => "--offline-threshold THRESHOLD",
        :default => 3,
        :description => "Number of missed intervals before I stop sending a heartbeat"

      option :online_threshold,
        :long => "--online-threshold THRESHOLD",
        :default => 2,
        :description => "Number of messages to receive after disconnect before I start sending a heartbeat"

      option :interval,
        :short => "-i INTERVAL",
        :long => "--interval INTERVAL",
        :default => 1,
        :description => "How often do I send a heartbeat"

      option :lifetime,
        :short => "-r TIMEOUT",
        :long => "--lifetime TIMEOUT",
        :default => 3600,
        :description => "How often do restart the client"

      option :out_address,
        :long => "--out-address HOST",
        :default => "tcp://127.0.0.1:10000",
        :description => "URL pointing to the server's heartbeat broadcast service"

      option :in_address,
        :long => "--in-address HOST",
        :default => "tcp://127.0.0.1:10001",
        :description => "URL pointing to the server's node state tracking service"

      option :config_service,
        :short => "-s HOST",
        :long => "--config-service HOST",
        :default => "localhost:10003/organizations/clownco",
        :description => "URL pointing to configuration service (eventually same as chef)"

      option :verbose,
        :short => "-v",
        :long => "--verbose",
        :boolean => true,
        :description => "Be verbose"

      option :client_private_key_path,
        :long => "--client-key KEY_FILE",
        :description => "Set the client key file location",
        :default => File.expand_path(File.join(File.dirname(__FILE__), '..', '..', '..', 'keys', 'client_private.pem')),
        :proc => nil

      option :server_public_key_path,
        :long => "--server-key KEY_FILE",
        :description => "Set the client key file location",
        :default => File.expand_path(File.join(File.dirname(__FILE__), '..', '..', '..', 'keys', 'server_public.pem')),
        :proc => nil

      option :help,
        :short => "-h",
        :long => "--help",
        :description => "Show this message",
        :on => :tail,
        :boolean => true,
        :show_options => true,
        :exit => 0

      option :log_level,
        :short => "-l LEVEL",
        :long  => "--log_level LEVEL",
        :description => "Set the log level (debug, info, warn, error, fatal) (default: warn)",
        :default => :error,
        :proc => Proc.new { |l| l.to_sym }

      def setup
        trap("TERM") do
        end

        trap("INT") do
          EM::stop()
        end

        trap("QUIT") do
          EM::stop()
        end

        trap("HUP") do
          reconfigure
        end

        self.parse_options

        # set log level
        Pushy::Log.level = config[:verbose] ? :debug : config[:log_level]

        self
      end

      def run
        reconfigure
        app = Pushy::App.new \
                :service_url_base        => config[:config_service],
                :client_private_key_path => config[:client_private_key_path],
                :node_name               => config[:node_name]

        app.start
      end

      def reconfigure
        unless ::File.exists? config[:config_file]
          Pushy::Log.warn "No config file. Using command line options."
          return
        end

        ::File::open(config[:config_file]) do |f|
          Chef::Config.from_file(f.path)
        end

        true
      end

    end
  end
end
