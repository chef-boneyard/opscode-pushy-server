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

require 'openssl'

ENV['PATH'] = "#{node['pushy']['install_path']}/bin:#{node['pushy']['install_path']}/embedded/bin:#{ENV['PATH']}"

directory "/etc/opscode" do
  owner "root"
  group "root"
  mode "0775"
  action :nothing
end.run_action(:create)

PushyServer[:node] = node
if File.exists?("/etc/opscode/opscode-pushy-server.rb")
  PushyServer.from_file("/etc/opscode/opscode-pushy-server.rb")
end
node.consume_attributes(PushyServer.generate_config(node['fqdn']))

if File.exists?("/var/opt/opscode/pushy-bootstrapped")
  node['pushy']['bootstrap']['enable'] = false
end

# Create the Chef User
include_recipe "opscode-pushy-server::users"

directory "/var/opt/opscode" do
  owner "root"
  group "root"
  mode "0755"
  recursive true
  action :create
end

# Install our runit instance
include_recipe "runit"

include_recipe "opscode-pushy-server::postgresql"

# Configure Services
[
  "opscode-pushy-server",
  "bootstrap"
].each do |service|
  if node["pushy"][service]["enable"]
    include_recipe "opscode-pushy-server::#{service}"
  else
    include_recipe "opscode-pushy-server::#{service}_disable"
  end
end

# TODO - JC
# include_recipe "opscode-pushy-server::pushy-pedant"

file "/etc/opscode/opscode-pushy-server-running.json" do
  owner node['pushy']['user']['username']
  group "root"
  mode "0644"
  content Chef::JSONCompat.to_json_pretty({ "pushy" => node['pushy'].to_hash, "run_list" => node.run_list })
end
