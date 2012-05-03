require File.dirname(__FILE__) + "/../client"

module Pushy
  module CLI
    class Client

      include Mixlib::CLI

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

        run
      end

      def run
        reconfigure
        client = Pushy::Client.new
        client.start
      end

      def reconfigure
        return if config[:config_file].empty?

        ::File::open(config[:config_file]) do |f|
          Chef::Config.from_file(f.path)
        end
      end

    end
  end
end
