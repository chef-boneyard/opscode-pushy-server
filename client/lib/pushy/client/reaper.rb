module Pushy
  class Reaper
    # Kills a client after specified lifetime

    attr_reader :lifetime, :client, :timer

    def watch!(options)
      new(options).tap(&:start)
    end

    def initialize(options)
      @lifetime = options[:lifetime]
      @client = options[:client]
    end

    def start
      @timer = EM::Timer.new(lifetime) do
        Pushy::Client.reload!(client)
      end
    end

    def stop
      @timer.cancel
    end
  end
end
