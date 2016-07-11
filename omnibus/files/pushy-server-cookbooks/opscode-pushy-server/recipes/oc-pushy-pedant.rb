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

# Snag the first supported protocol version by our ruby installation
ssl_protocols = node['private_chef']['nginx']['ssl_protocols']
supported_versions = OpenSSL::SSL::SSLContext::METHODS
allowed_versions = ssl_protocols.split(/ /).select do |proto|
  supported_versions.include? proto.gsub(".", "_").to_sym
end

# In a healthy installation, we should be able to count on
# at least one shared protocol version. Leaving failure unhandled here,
# since it means that a pedant run is not possible.
ssl_version = allowed_versions.first.gsub(".", "_").to_sym

template "/etc/opscode-push-jobs-server/pedant_config.rb" do
  owner "root"
  group "root"
  mode  "0755"
  variables({
              :api_url  => node['private_chef']['nginx']['url'],
              :default_orgname => node['private_chef']['default_orgname'],
              :hostname => node['hostname'],
              :ssl_version =>  ssl_version,
              :running_from_backend => is_data_master?})
end
