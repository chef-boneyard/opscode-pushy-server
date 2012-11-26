module Pedant
  class CommandLine
    attr_accessor :pushy_client_debug

    def to_hash
      rc = members
      rc.push(:pushy_client_debug)
      rc.inject({}) do |as_hash, option|
        as_hash[option] = send(option)
        as_hash
      end
    end

    def pushy_options(opts)
      opts.on("--client-debug",
              "Dump full pushy client debug logs to console") do |d|
        self.pushy_client_debug = d
      end
    end
  end
end
