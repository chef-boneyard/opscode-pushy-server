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

pushy_dir = node['pushy']['opscode-pushy-server']['dir']
pushy_etc_dir = File.join(pushy_dir, "etc")
pushy_log_dir = node['pushy']['opscode-pushy-server']['log_directory']
pushy_sasl_log_dir = File.join(pushy_log_dir, "sasl")
[
  pushy_dir,
  pushy_etc_dir,
  pushy_log_dir,
  pushy_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "#{node['pushy']['install_path']}/embedded/service/opscode-pushy-server/log" do
  to pushy_log_dir
end

template "#{node['pushy']['install_path']}/embedded/service/opscode-pushy-server/bin/opscode-pushy-server" do
  source "opscode-pushy-server.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['pushy']['opscode-pushy-server'].to_hash)
  notifies :restart, 'service[opscode-pushy-server]' if OmnibusHelper.should_notify?("opscode-pushy-server")
end

pushy_config = File.join(pushy_etc_dir, "app.config")

template pushy_config do
  source "opscode-pushy-server.config.erb"
  mode "644"
  variables(node['pushy']['opscode-pushy-server'].to_hash)
  notifies :restart, 'service[opscode-pushy-server]' if OmnibusHelper.should_notify?("opscode-pushy-server")
end

link "#{node['pushy']['install_path']}/embedded/service/opscode-pushy-server/etc/app.config" do
  to pushy_config
end

runit_service "opscode-pushy-server" do
  options({
    :log_directory => pushy_log_dir,
    :svlogd_size => node['pushy']['opscode-pushy-server']['svlogd_size'],
    :svlogd_num  => node['pushy']['opscode-pushy-server']['svlogd_num']
  }.merge(params))
end

if node['pushy']['bootstrap']['enable']
  execute "#{node['pushy']['install_path']}/bin/opscode-push-jobs-server-ctl start opscode-pushy-server" do
    retries 20
  end
end


