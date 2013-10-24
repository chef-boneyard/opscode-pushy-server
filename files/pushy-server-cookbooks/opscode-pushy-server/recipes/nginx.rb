#
# Author:: James Casey (<james@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, "etc")
nginx_addon_dir = File.join(nginx_etc_dir, "addon.d")

# This will already exist, thanks to Enterprise Chef.  We simply
# restate it here for notifying once we lay down some extra config
# files.
runit_service "nginx" do
  action :nothing
end

# Pushy LB configs
["upstreams", "external"].each do |config|
  file = File.join(nginx_addon_dir, "10-push_jobs_#{config}.conf")

  template file do
    source "nginx-#{config}.conf.erb"
    owner "root"
    group "root"
    mode "0644"
    notifies :restart, 'runit_service[nginx]' unless backend_secondary?
  end
end
