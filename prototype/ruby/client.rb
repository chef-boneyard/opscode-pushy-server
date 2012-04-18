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

# Connect to server
subscriber = context.socket(ZMQ::SUB)
subscriber.connect("#{server}:#{sub_port}")
# Disable filtering
subscriber.setsockopt(ZMQ::SUBSCRIBE, '')

#
# 
#

puts "Starting"

def listen_blocking(subscriber) 
  while true
    subscriber_msg= ''
    subscriber.recv_string(subscriber_msg)
    # process update
    puts "ping: #{subscriber_msg}"
  end
end

def listen_nonblocking(subscriber) 
  while true
    subscriber_msg= ''
    if subscriber.recv_string(subscriber_msg, ZMQ::NOBLOCK) && !subscriber_msg.empty?
      puts "ping: #{subscriber_msg}"
    end
    sleep 0.001
  end
end

def heartbeat_server(context, server, port)
  pusher = context.socket(ZMQ::PUSH)
  pusher.connect("#{server}:#{port}")
  sequence = 0
  host = (`hostname`).chomp
  id   = UUIDTools::UUID.random_create
  while true
    time = Time.now()
    update = "%08d %s %s %s" % [sequence, host, id, time]
    puts update
    pusher.send_string(update)
    sequence+=1
    sleep 1
  end
end

Thread.new{heartbeat_server(context, server, hb_port)}
listen_blocking(subscriber)



