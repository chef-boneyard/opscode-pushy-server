#!/usr/bin/ruby

#
# Shamelessly stolen from the zmq examples
#


require 'rubygems'
require 'ffi-rzmq'
require 'pp'
require 'uuidtools'

server =  "tcp://localhost"
sub_port = 5556
hb_port  = 5557

# 
context = ZMQ::Context.new(1)

class HeartbeatMonitor
  attr_reader   :expected_interval
  attr_reader   :hysteresis_count
 
  def initialize(interval, hysteresis)
    @expected_interval = interval
    @hysteresis_count = hysteresis
    @last_checkin = Time.new(0)
    @state_ok = false
    @ok_count = 0
    @mutex = Mutex.new
  end


  # We're in trouble if we've missed heartbeats.
  def missing_heartbeats
    (@last_checkin - Time.now()) / @expected_interval
  end

  def checkin()
    @mutex.synchronize do
      missing = missing_heartbeats
      if (missing > 1.2) 
        @ok_count -= missing.trunc
        if ok_count <= 0 then
          @state_ok = false
          @ok_count
        end
      else
      @ok_count += 1
        if @ok_count >= @hysteresis_count then
          @state_ok = true
          @ok_count = @hysteresis_count
        end
      end
      @last_checkin = Time.now()    
      puts "monitor state: #{@last_checkin} #{@state_ok} #{@ok_count}"
    end
  end

  def is_ok
    @mutex.synchronize do
      @state_ok
    end
  end

  def status 
    @mutex.synchronize do
      puts "monitor state: #{@last_checkin} #{@state_ok} #{@ok_count}"
    end
  end
end

#
# 
#

class HeartbeatListener 
  attr_accessor :server, :port
  attr_reader   :context
  attr_reader   :subscriber

  def initialize(context, server, port, monitor)
    @context = context
    @server = server
    @port = port
    @monitor = monitor
    connect()
  end

  def connect()
    # Connect to server
    puts "Connecting to #{@server}, #{@port}"
    @subscriber = context.socket(ZMQ::SUB)
    subscriber.connect("#{@server}:#{@port}")
    # Disable filtering
    subscriber.setsockopt(ZMQ::SUBSCRIBE, '')
  end

  def listen_blocking() 
    while true
      subscriber_msg= ''
      subscriber.recv_string(subscriber_msg)
      # process update
      @monitor.checkin
      puts "hb in: #{subscriber_msg}"
    end
  end

  def listen_nonblocking() 
    while true
      subscriber_msg= ''
      if subscriber.recv_string(subscriber_msg, ZMQ::NOBLOCK) && !subscriber_msg.empty?
        puts "hb in: #{subscriber_msg}"
        @monitor.checkin
      else
        @monitor.status
      end
      sleep 0.5
    end
  end
end

class HeartbeatGenerator
  attr_accessor :server, :port
  
  def initialize(context, server, port, monitor)
    @context = context
    @server = server
    @port = port
    @monitor = monitor
    @pusher = context.socket(ZMQ::PUSH)
    @pusher.connect("#{server}:#{port}")
    @sequence = 0
  end
  
  def go
    host = (`hostname`).chomp
    id   = UUIDTools::UUID.random_create
    puts "Starting generator #{host} #{id}"
    while true
      time = Time.now()
      update = "%08d %s %s %s" % [@sequence, host, id, time]
      if @monitor.is_ok() then
        puts "hb out: #{update}"
        @pusher.send_string(update)
        @sequence+=1
      else 
        puts "hb out: suspended #{update}"
      end
      sleep 1
    end
    puts "Exiting generator #{host} #{id}"
  end
end

monitor = HeartbeatMonitor.new(1,5)
generator = HeartbeatGenerator.new(context, server, hb_port, monitor)
Thread.new{generator.go}
hbl = HeartbeatListener.new(context, server, sub_port, monitor)
hbl.listen_blocking()

