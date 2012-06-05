module Pushy
  class Reaper
    # Kills a client after specified lifetime

    attr_reader :lifetime, :app, :timer

    def self.watch!(options)
      new(options).tap(&:start)
    end

    def initialize(options)
      @lifetime = options[:lifetime]
      @app = options[:app]
    end

    def start
      Pushy::Log.info "Reaper: will reap in #{lifetime} seconds"
      @timer = EM::Timer.new(lifetime) do
        Pushy::Log.info "Reaper: Timeout (#{lifetime}) reached, killing and restart client"
        app.reload
      end
    end

    def stop
      @timer.cancel
    end
  end
end
