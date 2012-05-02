
module Pushy
  module CLI
    class Client

      option :config_file,
        :short => "-c CONFIG",
        :long => "--config CONFIG",
        :default => '',
        :description => "The configuration file to use"

      def initialize
        super

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
      end

      def run
        reconfigure
        ctx = EM::ZeroMQ::Context.new(1)
        client = Pushy::Client.new(ctx)
        client.start
      end

      def reconfigure
        ::File::open(config[:config_file]) do |f|
          Chef::Config.from_file(f.path)
        end
      end

    end
  end
end
