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
require 'chef/mixin/deep_merge'

module PushJobsServer
  extend(Mixlib::Config)

  opscode_pushy_server Mash.new
  postgresql Mash.new
  bootstrap Mash.new
  oc_chef_pedant Mash.new

  class << self
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

      results['pushy']['oc-chef-pedant'] = PushJobsServer['oc_chef_pedant']

      results
    end

    def generate_config(node_name)
      # inherit postgres config from chef-server
      PushJobsServer['postgresql']['vip']           = node['private_chef']['postgresql']['vip']
      PushJobsServer['postgresql']['port']          = node['private_chef']['postgresql']['port']
      PushJobsServer['postgresql']['db_superuser']  = node['private_chef']['postgresql']['db_superuser']
      PushJobsServer['postgresql']['sslmode']       = node['private_chef']['postgresql']['sslmode']

      topology = node['private_chef']['topology']
      case topology
      when "ha", "tier"
        PushJobsServer['opscode_pushy_server']['ha'] = (topology == 'ha')
        PushJobsServer['opscode_pushy_server']['vip'] ||= node['private_chef']['backend_vips']['ipaddress']

        case node['private_chef']['servers'][node_name]['role']
        when "backend"
          # nothing special needs to be done
        when "frontend"
          gen_frontend
        else
          raise "I don't have a role for you!  Use 'backend' or 'frontend'."
        end
      else
        PushJobsServer['opscode_pushy_server']['vip'] ||= node['private_chef']['lb']['api_fqdn']
      end
      generate_hash
    end
  end
end
