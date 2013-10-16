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
  database_exists = "#{node['pushy']['install_path']}/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select datname from pg_database' -x|grep opscode_pushy"
  user_exists     = "#{node['pushy']['install_path']}/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select usename from pg_user' -x|grep #{node['pushy']['postgresql']['sql_user']}"
  ro_user_exists  = "#{node['pushy']['install_path']}/embedded/bin/chpst -u #{node['pushy']['postgresql']['username']} #{psql_cmd} -d 'template1' -c 'select usename from pg_user' -x|grep #{node['pushy']['postgresql']['sql_ro_user']}"

  execute "#{createdb_cmd} -T template0 -E UTF-8 opscode_pushy" do
    user node['pushy']['postgresql']['username']
    not_if database_exists
    retries 30
  end

  execute "pushy_schema" do
    # The version of the schema to be deployed will the the maximum
    # available in the oc_bifrost repository.  This will be the same
    # version needed by the code that is deployed here.  If we ever
    # split bifrost's code and schema into separate repositories,
    # we'll need to deploy to a specific schema tag
    command <<-EOM.gsub(/\s+/," ").strip!
    sqitch --engine pg
           --db-name opscode_pushy
           --top-dir #{node['pushy']['install_path']}/embedded/service/pushy-server-schema
           deploy --verify
    EOM
    user node['pushy']['postgresql']['username']
    # If sqitch is deploying the first time, it'll return 0 on
    # success.  If it's running a second time and ends up deploying
    # nothing (since we've already deployed all changesets), it'll
    # return 1.  Both scenarios should be considered successful.
    returns [0,1]
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
