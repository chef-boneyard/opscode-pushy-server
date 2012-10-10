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

###
# High level options
###
default['pushy']['install_path'] = "/opt/opscode-push-jobs-server"

default['pushy']['database_type'] = "postgresql"
default['pushy']['bootstrap']['enable'] = true
default['pushy']['chef_base_path'] = "/opt/opscode"

####
# The User that services run as
####
# The username for the pushy services user
default['pushy']['user']['username'] = "opscode"
# The shell for the pushy services user
default['pushy']['user']['shell'] = "/bin/sh"
# The home directory for the pushy services user
default['pushy']['user']['home'] = "/opt/opscode/embedded"

####
# Pushy Server
####
default['pushy']['opscode-pushy-server']['enable'] = true
default['pushy']['opscode-pushy-server']['dir'] = "/var/opt/opscode/opscode-pushy-server"
default['pushy']['opscode-pushy-server']['log_directory'] = "/var/log/opscode/opscode-pushy-server"
default['pushy']['opscode-pushy-server']['listen'] = '127.0.0.1'
default['pushy']['opscode-pushy-server']['db_pool_size'] = '20'

default['pushy']['opscode-pushy-server']['heartbeat_interval'] = '1000'
default['pushy']['opscode-pushy-server']['dead_interval'] = '3'

default['pushy']['opscode-pushy-server']['zeromq_listen_address'] = 'tcp://*'
default['pushy']['opscode-pushy-server']['zmq_io_processes'] = '1'

default['pushy']['opscode-pushy-server']['server_heartbeat_port'] = '10000'
default['pushy']['opscode-pushy-server']['command_port'] = '10002'
default['pushy']['opscode-pushy-server']['api_port'] = '10003'
default['pushy']['opscode-pushy-server']['keyring_dir'] = '/etc/opscode'

###
# PostgreSQL
###
default['pushy']['postgresql']['username'] = "opscode-pgsql"
default['pushy']['postgresql']['sql_user'] = "opscode_pushy"
default['pushy']['postgresql']['sql_password'] = "snakepliskin"
default['pushy']['postgresql']['sql_ro_user'] = "opscode_pushy_ro"
default['pushy']['postgresql']['sql_ro_password'] = "shmunzeltazzen"
default['pushy']['postgresql']['vip'] = "127.0.0.1"
default['pushy']['postgresql']['port'] = 5432
