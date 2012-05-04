#
# Binds PUB socket to tcp://*:5556
#

require 'rubygems'
require 'ffi-rzmq'


listen_if = "tcp://*"
pub_port  = 5556
hb_port   = 5557

context = ZMQ::Context.new(1) # one io thread

def server_ping(context, listen_if, port)
  publisher = context.socket(ZMQ::PUB)
  publisher.bind("#{listen_if}:#{port}")
  
  sequence = 0
  server = (`hostname`).chomp!
  while true
    time = Time.now()
    update = "%08d %s %s" % [sequence, server, time]
    puts "ud: #{update}"
    publisher.send_string("A", ZMQ::SNDMORE)
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
