#
# Binds PUB socket to tcp://*:5556
#

require 'rubygems'
require 'ffi-rzmq'
require 'yajl'


listen_if = "tcp://*"
pub_port  = 10000
hb_port   = 10001

context = ZMQ::Context.new(1) # one io thread

def server_ping(context, listen_if, port)
  publisher = context.socket(ZMQ::PUB)
  publisher.bind("#{listen_if}:#{port}")
  
  sequence = 0
  server = (`hostname`).chomp!

  hash = { type: 'heartbeat',
           host: server }

  encoder = Yajl::Encoder.new
  json = encoder.encode hash

  while true
    time = Time.now()
    update = "%08d %s %s" % [sequence, server, time]
    puts "ud: #{update}"
    publisher.send_string("Validation", ZMQ::SNDMORE)
    publisher.send_string(json, ZMQ::SNDMORE)
    publisher.send_string(update)
    sequence+=1
    sleep 1
  end
end

def hb_collect(context, listen_if, port)
  hb_pull = context.socket(ZMQ::PULL)
  hb_pull.bind("#{listen_if}:#{port}")

  while true
    msg= ''
    hb_pull.recv_string(msg)
    # process update
    puts "hb: #{msg}"
  end
end


Thread.new{server_ping(context, listen_if, pub_port)}
hb_collect(context, listen_if, hb_port)
