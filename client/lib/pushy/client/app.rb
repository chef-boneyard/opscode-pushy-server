require 'time'
require 'pp'

module Pushy
  class App
    DEFAULT_SERVICE_URL_BASE = "localhost:10003/organization/clownco"

    attr_accessor :service_url_base
    attr_accessor :client_private_key_path
    attr_accessor :node_name

    attr_accessor :reaper, :worker


    def initialize(options)
      @service_url_base        = options[:service_url_base]
      @client_private_key_path = options[:client_private_key_path]
      @node_name               = options[:node_name]

      Pushy::Log.info "Using configuration endpoint: #{service_url_base}"
      Pushy::Log.info "Using private key: #{client_private_key_path}"
      Pushy::Log.info "Using node name: #{node_name}"
    end

    def start
      Pushy::Log.info "Booting ..."

      EM.run do
        start_worker
      end

    end

    def stop
      Pushy::Log.info "Stopping client ..."
      worker.stop
      Pushy::Log.info "Stopped."
    end

    def reload
      worker.stop
      start_worker
    end

    def start_worker
      self.worker = Pushy::Worker.load!(self).tap(&:start)
      self.reaper = Pushy::Reaper.watch! :app => self, :lifetime => worker.lifetime
    end

  end
end
