module Pushy
  class Client
    def initialize(config)
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

        # Listen for a response
        pull_socket = ctx.socket(ZMQ::PULL, Pushy::Handler.new)
        pull_socket.connect("tcp://#{@config[:address]}:#{@config[:port]}")

      end
    end
  end

end
