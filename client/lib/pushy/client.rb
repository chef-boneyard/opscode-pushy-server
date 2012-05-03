module Pushy
  class Handler
    attr_reader :received
    def on_readable(socket, parts)
      parts.each do |part|
        puts "YAY #{part.copy_out_string}"
      end
    end
  end

  class Client
    def initialize
      @ctx = EM::ZeroMQ::Context.new(1)
    end

    def start
      ctx = @ctx


      EM.run do

        subscriber = ctx.socket(ZMQ::SUB)
        subscriber.connect("tcp://localhost:5556")

        pull_socket = ctx.socket(ZMQ::PULL, Pushy::Handler.new)
        pull_socket.connect('tcp://localhost:5556')

      end
    end

  end
end
