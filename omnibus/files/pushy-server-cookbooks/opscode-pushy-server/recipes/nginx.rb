#
# Author:: James Casey (<james@opscode.com>)
#

# @copyright Copyright 2013 Chef Software, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, "etc")
nginx_addon_dir = File.join(nginx_etc_dir, "addon.d")

# This will already exist, thanks to Enterprise Chef.  We simply
# restate it here for notifying once we lay down some extra config
# files.
runit_service "nginx" do
  service_dir "/opt/opscode/service"
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
