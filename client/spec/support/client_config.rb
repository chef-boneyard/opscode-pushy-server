module SpecHelpers
  module Config
    extend SpecHelpers::Concern


    included do
      let(:host) { 'opc1.opscode.com' }
      let(:out_addr) { 'tcp://10.10.1.5:10000' }
      let(:in_addr)  { 'tcp://10.10.1.5:10001' }
      let(:interval) { 15 }
      let(:offline_threshold) { 3 }
      let(:online_threshold) { 2 }
      let(:lifetime) { 3600 }

      let(:public_key) { <<-PUB_KEY }
AAAAB3NzaC1kc3MAAACBAIZbwlySffbB
5msSUH8JzLLXo/v03JBCWr13fVTjWYpc
cdbi/xL3IK/Jw8Rm3bGhnpwCAqBtsLvZ
OcqXrc2XuKBYjiKWzigBMC7wC9dUDGwDl
2aZ89B0jn2QPRWZuCAkxm6sKpefu++VPR
RZF+iyZqFwS0wVKtl97T0gwWlzAJYpAAA
AFQDIipDNo83e8RRp7Fits0DSy0DCpwAA
AIB01BwXg9WSfU0mwzz/0+5Gb/TMAxfkD
yucbcpJNncpRtr9Jb+9GjeZIbqkBQAqwg
dbEjviRbUAuSawNSCdtnMgWD2NXkBKEde
PUB_KEY

      let(:config_json) { <<-JSON }
   {
      "type": "config",
      "host": "#{host}",
      "push_jobs": {
                     "heartbeat": {
                                    "out_addr": "#{out_addr}",
                                    "in_addr": "#{in_addr}",
                                    "interval": #{interval},
                                    "offline_threshold": #{offline_threshold},
                                    "online_threshold": #{online_threshold}
                                  }
                   },
      "public_key": #{public_key.to_json},
      "lifetime": #{lifetime}
    }
JSON

    end

  end
end
