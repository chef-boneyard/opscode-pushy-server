module SpecHelpers
  module Config
    extend SpecHelpers::Concern


    included do
      let(:config_json) { <<-JSON }
   {
      "type": "config",
      "host": "opc1.opscode.com",
      "push_jobs": {
                     "heartbeat": {
                                    "out_addr": "tcp://10.10.1.5:10000",
                                    "in_addr": "tcp://10.10.1.5:10001",
                                    "interval": 15,
                                    "offline_threshold": 3,
                                    "online_threshold": 2
                                  },
                   },
      "public_key": "AAAAB3NzaC1kc3MAAACBAIZbwlySffbB
                    5msSUH8JzLLXo/v03JBCWr13fVTjWYpc
                    cdbi/xL3IK/Jw8Rm3bGhnpwCAqBtsLvZ
                    OcqXrc2XuKBYjiKWzigBMC7wC9dUDGwDl
                    2aZ89B0jn2QPRWZuCAkxm6sKpefu++VPR
                    RZF+iyZqFwS0wVKtl97T0gwWlzAJYpAAA
                    AFQDIipDNo83e8RRp7Fits0DSy0DCpwAA
                    AIB01BwXg9WSfU0mwzz/0+5Gb/TMAxfkD
                    yucbcpJNncpRtr9Jb+9GjeZIbqkBQAqwg
                    dbEjviRbUAuSawNSCdtnMgWD2NXkBKEde",
       "lifetime":3600

    }
      JSON
    end

  end
end
