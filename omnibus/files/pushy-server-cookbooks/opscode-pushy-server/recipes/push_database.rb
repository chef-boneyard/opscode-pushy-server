#
# Copyright:: Copyright (c) 2015 Opscode, Inc.
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


postgres_attrs = node['private_chef']['postgresql']
install_path = node['pushy']['install_path']
push_attrs = node['pushy']['postgresql']
database_name = node['pushy']['postgresql']['database_name']

# TODO - check for postgres to be up and accessible

# create users

opscode_pushy_server_pg_user push_attrs['sql_user'] do
  password push_attrs['sql_password']
  superuser false
end

opscode_pushy_server_pg_user push_attrs['sql_ro_user'] do
  password push_attrs['sql_ro_password']
  superuser false
end

opscode_pushy_server_pg_database database_name do
  owner push_attrs['sql_user']
end

opscode_pushy_server_pg_user_table_access push_attrs['sql_user'] do
  database database_name
  schema 'public'
  access_profile :write
end

opscode_pushy_server_pg_user_table_access push_attrs['sql_ro_user'] do
  database database_name
  schema 'public'
  access_profile :read
end

# TODO Fix this hardcoded path!
opscode_pushy_server_pg_sqitch  "#{install_path}/embedded/service/pushy-server-schema" do
  hostname postgres_attrs['vip']
  port     postgres_attrs['port']
  username  postgres_attrs['db_superuser']
  password  postgres_attrs['db_superuser_password']
  database database_name
end



