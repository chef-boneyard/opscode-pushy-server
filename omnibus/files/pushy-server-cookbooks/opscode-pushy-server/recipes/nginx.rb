#
# Author:: James Casey (<james@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, "etc")
nginx_addon_dir = File.join(nginx_etc_dir, "addon.d")

# Pushy LB configs
["upstreams", "external"].each do |config|
  file = File.join(nginx_addon_dir, "10-push_jobs_#{config}.conf")

  template file do
    source "nginx-#{config}.conf.erb"
    owner "root"
    group "root"
    mode "0644"
    notifies :restart, 'service[nginx]' if OmnibusHelper.should_notify?("nginx",
                                                                        node['pushy']['chef_base_path'],
                                                                        "private-chef-ctl")
  end
end

service "nginx" do
    control_cmd = "#{node['pushy']['chef_base_path']}/embedded/bin/sv"
    provider Chef::Provider::Service::Simple
    supports :restart => true, :status => true
    restart_command "#{control_cmd} restart nginx"
    action :nothing
end
