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

# Prefer pushy executables, then enterprise-chef executables, then system executables
ENV['PATH'] = "#{node['pushy']['install_path']}/bin:#{node['pushy']['install_path']}/embedded/bin:/opt/opscode/bin:/opt/opscode/embedded/bin:#{ENV['PATH']}"

directory "/etc/opscode-push-jobs-server" do
  owner "root"
  group "root"
  mode "0775"
  action :nothing
end.run_action(:create)

# We need to load the private chef configuration
if File.exists?("/etc/opscode/chef-server-running.json")
  private_chef = JSON.parse(IO.read("/etc/opscode/chef-server-running.json"))
end
node.consume_attributes({
  'private_chef' => private_chef['private_chef'],
  'runit'        => private_chef['runit']
})

PushJobsServer[:node] = node
if File.exists?("/etc/opscode-push-jobs-server/opscode-push-jobs-server.rb")
  PushJobsServer.from_file("/etc/opscode-push-jobs-server/opscode-push-jobs-server.rb")
end
node.consume_attributes(PushJobsServer.generate_config(node['fqdn']))

if File.exists?("/var/opt/opscode-push-jobs-server/bootstrapped")
  node.set['pushy']['bootstrap']['enable'] = false
end

pushy_key = OpenSSL::PKey::RSA.generate(2048) unless File.exists?('/etc/opscode-push-jobs-server/pushy_pub.pem')

file "/etc/opscode-push-jobs-server/pushy_pub.pem" do
  owner "root"
  group "root"
  mode "0644"
  content pushy_key.public_key.to_s unless File.exists?('/etc/opscode-push-jobs-server/pushy_pub.pem')
end

file "/etc/opscode-push-jobs-server/pushy_priv.pem" do
  owner node["private_chef"]["user"]["username"]
  group "root"
  mode "0600"
  content pushy_key.to_pem.to_s unless File.exists?('/etc/opscode-push-jobs-server/pushy_pub.pem')
end

directory "/var/opt/opscode-push-jobs-server" do
  owner "root"
  group "root"
  mode "0755"
  recursive true
  action :create
end

# Install our runit instance

include_recipe "enterprise::runit"

# TODO Figure out why is_data_master is returning false in standalone mode...
include_recipe "opscode-pushy-server::push_database" #if is_data_master?

include_recipe "opscode-pushy-server::nginx"

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
include_recipe "opscode-pushy-server::oc-pushy-pedant"

file "/etc/opscode-push-jobs-server/opscode-push-jobs-server-running.json" do
  owner node['private_chef']['user']['username']
  group "root"
  mode "0600"
  content Chef::JSONCompat.to_json_pretty({ "pushy" => node['pushy'].to_hash, "run_list" => node.run_list })
end
