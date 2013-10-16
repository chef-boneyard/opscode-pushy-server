#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'mixlib/config'
require 'chef/mash'
require 'chef/json_compat'
require 'chef/mixin/deep_merge'
require 'securerandom'

module PushJobsServer
  extend(Mixlib::Config)

  opscode_pushy_server Mash.new
  postgresql Mash.new
  bootstrap Mash.new

  class << self

    # guards against creating secrets on non-bootstrap node
    def generate_hex(chars)
      SecureRandom.hex(chars)
    end

    def generate_secrets(node_name)
      existing_secrets ||= Hash.new
      if File.exists?("/etc/opscode-push-jobs-server/opscode-push-jobs-server-secrets.json")
        existing_secrets = Chef::JSONCompat.from_json(File.read("/etc/opscode-push-jobs-server/opscode-push-jobs-server-secrets.json"))
      end
      existing_secrets.each do |k, v|
        v.each do |pk, p|
          PushJobsServer[k][pk] = p
        end
      end

      PushJobsServer['postgresql']['sql_password'] ||= generate_hex(50)
      PushJobsServer['postgresql']['sql_ro_password'] ||= generate_hex(50)

      if File.directory?("/etc/opscode-push-jobs-server")
        File.open("/etc/opscode-push-jobs-server/opscode-push-jobs-server-secrets.json", "w") do |f|
          f.puts(
            Chef::JSONCompat.to_json_pretty({
              'postgresql' => {
                'sql_password' => PushJobsServer['postgresql']['sql_password'],
                'sql_ro_password' => PushJobsServer['postgresql']['sql_ro_password']
              }
            })
          )
          system("chmod 0600 /etc/opscode-push-jobs-server/opscode-push-jobs-server-secrets.json")
        end
      end
    end

    def gen_frontend
      PushJobsServer['bootstrap']['enable'] ||= false
      PushJobsServer['opscode_pushy_server']['enable'] ||= false
    end

    def generate_hash
      results = { "pushy" => {} }
      [
        "opscode_pushy_server",
        "postgresql",
        "bootstrap"
      ].each do |key|
        rkey = key.gsub('_', '-')
        results['pushy'][rkey] = PushJobsServer[key]
      end

      results
    end

    def generate_config(node_name)
      generate_secrets(node_name)
      case node['private_chef']['topology']
      when "ha"
        PushJobsServer['opscode_pushy_server']['vip'] =
          node['private_chef']['backend_vips']['ipaddress']
        case node['private_chef']['servers'][node_name]['role']
        when "backend"
          # nothing special needs to be done
        when "frontend"
          gen_frontend
        else
          raise "I don't have a role for you!  Use 'backend' or 'frontend'."
        end
      else
        # do nothing
      end
      generate_hash
    end
  end
end
