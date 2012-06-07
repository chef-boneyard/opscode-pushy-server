module Pushy
  class Monitor
    def initialize(options)
      @on_threshold = options[:online_threshold]
      @off_threshold = options[:offline_threshold]
      @interval = options[:interval]
      @on_counter = @off_counter = 0
      @online = false
      @callbacks = {}
    end

    def callback(type, &block)
      @callbacks[type.to_sym] ||= []
      @callbacks[type.to_sym] << block
    end

    def checkin!
      @off_counter = 0

      if @on_counter > @on_threshold
        fire_callback(:after_online) if @online == false
        @online = true
      else
        @on_counter += 1
      end
    end

    def online?
      @online
    end

    def start
      @timer = EM::PeriodicTimer.new(@interval) do
        if @off_counter > @off_threshold
          reset!
          @online = false
        else
          @off_counter += 1
        end
      end
    end

    def stop
      @timer.cancel
    end

    def reset!
      @on_counter = @off_counter = 0
    end

    private

    def fire_callback(type)
      if callables = @callbacks[type.to_sym]
        callables.each { |callable| callable.call }
      end
    end

  end
end
