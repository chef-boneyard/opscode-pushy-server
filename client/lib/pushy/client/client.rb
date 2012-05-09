module Pushy
  class Client
    attr_accessor :monitor
    attr_accessor :ctx
    attr_accessor :out_address
    attr_accessor :in_address
    attr_accessor :interval

    def initialize(options)
      @monitor = Pushy::Monitor.new(options)
      @ctx = EM::ZeroMQ::Context.new(1)
      @out_address = options[:out_address]
      @in_address = options[:in_address]
      @interval = options[:interval]
    end

    def start
      Pushy::Log.info "Listening at #{out_address}"

      EM.run do

        # Subscribe to heartbeat from the server
        subscriber = ctx.socket(ZMQ::SUB, Pushy::Handler.new(monitor))
        subscriber.connect(out_address)
        subscriber.setsockopt(ZMQ::SUBSCRIBE, "")

        # Push heartbeat to server
        push_socket = ctx.socket(ZMQ::PUSH)
        push_socket.connect(in_address)

        monitor.start

        EM::PeriodicTimer.new(interval) do
          if monitor.online?
            Pushy::Log.debug "Sending Message"
            encoder = Yajl::Encoder.new
            json = encoder.encode({:node => (`hostname`).chomp!})

            push_socket.send_msg("auth", json)
          end
        end

      end

    end

  end
end
