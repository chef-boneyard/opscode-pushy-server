module Pushy
  class Client
    attr_accessor :monitor
    attr_accessor :ctx
    attr_accessor :out_address
    attr_accessor :in_address
    attr_accessor :interval
    attr_accessor :orgname


    def initialize(options)
      @monitor = Pushy::Monitor.new(options)
      @ctx = EM::ZeroMQ::Context.new(1)
      @out_address = options[:out_address]
      @in_address = options[:in_address]
      @interval = options[:interval]
      @orgname = options[:orgname]
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
        push_socket.setsockopt(ZMQ::LINGER, 0)
        push_socket.connect(in_address)

        monitor.start

        seq = 0

        EM::PeriodicTimer.new(interval) do
          if monitor.online?
            encoder = Yajl::Encoder.new

            auth = encoder.encode({:version => 0.01,
                                   :checksum => checksum})

            json = encoder.encode({:node => (`hostname`).chomp!,
                                   :client => (`hostname`).chomp!,
                                   :org => orgname,
                                   :sequence => seq,
                                   :timestamp => Time.now})

            Pushy::Log.debug "Sending Message #{json}"
            push_socket.send_msg(auth, json)

            seq += 1
          end
        end

      end

    end

    private

    def checksum
      "12345"
    end

  end
end
