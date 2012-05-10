module Pushy
  class Handler

    attr_reader :received
    attr_accessor :monitor

    def initialize(monitor)
      @monitor = monitor
    end

    def on_readable(socket, parts)
      require 'pp'

      puts parts[0].copy_out_string
      monitor.checkin!

      #puts parts.each do |part|
        #pp part
        ##part.each do |p|
          ##puts p.inspect
        ##end
      #end
      #if valid?(parts[0].copy_out_string)
        #monitor.checkin!
        #parse_json parts[1].copy_out_string
      #end
    end

    private

    def valid?(auth)
      true
    end

    def parse_json(json)
      parser = Yajl::Parser.new
      hash = parser.parse(json)

      puts hash['type']
      puts hash['host']
    end

  end
end
