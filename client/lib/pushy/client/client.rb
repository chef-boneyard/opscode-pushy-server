module Pushy
  class Client
    def initialize(config)
    attr_accessor :monitor
      @monitor = Pushy::Monitor.new(options)
      @ctx = EM::ZeroMQ::Context.new(1)
      @config = config
    end

    def start
      ctx = @ctx

      Pushy::Log.info "Listening at tcp://#{@config[:address]}:#{@config[:port]}"


      EM.run do

        # Subscribe to heartbeat from the server
        subscriber = ctx.socket(ZMQ::SUB)
        subscriber.connect("tcp://#{@config[:address]}:#{@config[:port]}")

        # Push heartbeat to server
        #push_socket = ctx.socket(ZMQ::PUSH)
        #push_socket.bind("tcp://#{@config[:address]}:#{@config[:port]}")
        monitor.start

        # Listen for a response
        pull_socket = ctx.socket(ZMQ::PULL, Pushy::Handler.new)
        pull_socket.connect("tcp://#{@config[:address]}:#{@config[:port]}")
        EM::PeriodicTimer.new(interval) do
          if monitor.online?
            Pushy::Log.debug "Sending Message"
            push_socket.send_msg("message")
          end
        end

      end
    end
  end

end
