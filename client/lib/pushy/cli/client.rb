module Pushy
  module CLI
    class Client

      include Mixlib::CLI

      option :config_file,
        :short => "-c CONFIG",
        :long => "--config CONFIG",
        :default => "config.rb",
        :description => "The configuration file to use"

      option :port,
        :short => "-p PORT",
        :long => "--port PORT",
        :default => "5556",
        :description => "use PORT (default: 5555)"

      option :address,
        :short => "-a HOST",
        :long => "--address HOST",
        :default => "0.0.0.0",
        :description => "bind to HOST address (default: 0.0.0.0)"

      option :verbose,
        :short => "-v",
        :long => "--verbose",
        :boolean => true,
        :description => "Be verbose"

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
        client = Pushy::Client.new(config)
        client.start
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
