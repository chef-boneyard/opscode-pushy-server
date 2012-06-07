require 'eventmachine'

module Pushy
  module Handler
    class Heartbeat

      attr_reader :received
      attr_accessor :monitor

      def initialize(monitor, client)
        @monitor = monitor
        @client = client
      end

      def on_readable(socket, parts)


        if valid?(parts)
          monitor.checkin!
          Utils.parse_json(parts[1].copy_out_string)
        end

      end

      private

      def valid?(parts)
        Utils.valid?(parts, @client.server_public_key)
      end

    end

    class Command

      def initialize(worker)
        @worker = worker
      end

      def on_readable(socket, parts)
        return unless valid?(parts)
        command_hash = Utils.parse_json(parts[1].copy_out_string)
        @worker.change_state "running"
        @worker.send_command_message(:started)
        ap command_hash
        command = EM::DeferrableChildProcess.open(command_hash['command'])
        command.callback do |data_from_child|
          puts data_from_child
          @worker.change_state "idle"
          @worker.send_command_message(:finished, command_hash['job_id'])
        end
      end

      private

      def valid?(parts)
        Utils.valid?(parts, @worker.server_public_key)
      end

    end

    module Utils

      def self.valid?(parts, server_public_key)
        auth = parts[0].copy_out_string.split(':')[2]
        body = parts[1].copy_out_string

        pp auth
        pp body

        decrypted_checksum = server_public_key.public_decrypt(Base64.decode64(auth))
        hashed_body = Mixlib::Authentication::Digester.hash_string(body)

        decrypted_checksum == hashed_body
      end

      def self.parse_json(json)
        Yajl::Parser.new.parse(json).tap do |body_hash|
          ap body_hash
        end
      end

    end
  end
end

