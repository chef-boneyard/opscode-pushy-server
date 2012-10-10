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

psql_cmd = "#{node['pushy']['chef_base_path']}/embedded/bin/psql"
createdb_cmd = "#{node['pushy']['chef_base_path']}/embedded/bin/createdb"

if node['pushy']['bootstrap']['enable']
  # TODO - check for postgres to be up and accessible

  ###
  # Create the database, migrate it, and create the users we need, and grant them
  # privileges.
  ###
  database_exists = "/opt/pushy-server/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select datname from pg_database' -x|grep opscode_pushy"
  user_exists     = "/opt/pushy-server/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select usename from pg_user' -x|grep #{node['pushy']['postgresql']['sql_user']}"
  ro_user_exists  = "/opt/pushy-server/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select usename from pg_user' -x|grep #{node['pushy']['postgresql']['sql_ro_user']}"

  execute "#{createdb_cmd} -T template0 -E UTF-8 opscode_pushy" do
    user node['pushy']['postgresql']['username']
    not_if database_exists
    retries 30
    notifies :run, "execute[migrate_database]", :immediately
  end

  execute "migrate_database" do
    command "#{psql_cmd} opscode_pushy < pgsql_schema.sql"
    cwd "#{node['pushy']['install_path']}/embedded/service/opscode-pushy-server/db"
    user node['pushy']['postgresql']['username']
    action :nothing
  end

  execute "#{psql_cmd} -d 'opscode_pushy' -c \"CREATE USER #{node['pushy']['postgresql']['sql_user']} WITH SUPERUSER ENCRYPTED PASSWORD '#{node['pushy']['postgresql']['sql_password']}'\"" do
    user node['pushy']['postgresql']['username']
    notifies :run, "execute[grant opscode_pushy privileges]", :immediately
    not_if user_exists
  end

  execute "grant opscode_pushy privileges" do
    command "#{psql_cmd} -d 'opscode_pushy' -c \"GRANT ALL PRIVILEGES ON DATABASE opscode_pushy TO #{node['pushy']['postgresql']['sql_user']}\""
    user node['pushy']['postgresql']['username']
    action :nothing
  end

  execute "#{psql_cmd} -d 'opscode_pushy' -c \"CREATE USER #{node['pushy']['postgresql']['sql_ro_user']} WITH SUPERUSER ENCRYPTED PASSWORD '#{node['pushy']['postgresql']['sql_ro_password']}'\"" do
    user node['pushy']['postgresql']['username']
    notifies :run, "execute[grant opscode_pushy_ro privileges]", :immediately
    not_if ro_user_exists
  end

  execute "grant opscode_pushy_ro privileges" do
    command "#{psql_cmd} -d 'opscode_pushy' -c \"GRANT ALL PRIVILEGES ON DATABASE opscode_pushy TO #{node['pushy']['postgresql']['sql_ro_user']}\""
    user node['pushy']['postgresql']['username']
    action :nothing
  end
end
