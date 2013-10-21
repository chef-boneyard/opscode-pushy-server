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

default['pushy']['bootstrap']['enable'] = true
default['pushy']['chef_base_path'] = "/opt/opscode"

####
# Pushy Server
####
default['pushy']['opscode-pushy-server']['enable'] = true
default['pushy']['opscode-pushy-server']['dir'] = "/var/opt/opscode-push-jobs-server/opscode-pushy-server"
default['pushy']['opscode-pushy-server']['log_directory'] = "/var/log/opscode-push-jobs-server/opscode-pushy-server"
default['pushy']['opscode-pushy-server']['listen'] = '127.0.0.1'
default['pushy']['opscode-pushy-server']['db_pool_size'] = '20'

default['pushy']['opscode-pushy-server']['ibrowse_max_sessions'] = 256
default['pushy']['opscode-pushy-server']['ibrowse_max_pipeline_size'] = 1

default['pushy']['opscode-pushy-server']['heartbeat_interval'] = '1000'

default['pushy']['opscode-pushy-server']['zeromq_listen_address'] = 'tcp://*'
default['pushy']['opscode-pushy-server']['zmq_io_processes'] = '1'

default['pushy']['opscode-pushy-server']['vip'] = '127.0.0.1'
default['pushy']['opscode-pushy-server']['server_heartbeat_port'] = '10000'
default['pushy']['opscode-pushy-server']['command_port'] = '10002'
default['pushy']['opscode-pushy-server']['api_port'] = '10003'
default['pushy']['opscode-pushy-server']['down_threshold'] = '0.6'
default['pushy']['opscode-pushy-server']['decay_window'] = '4'
default['pushy']['opscode-pushy-server']['detect_offline_nodes_interval'] = '4'
default['pushy']['opscode-pushy-server']['validate_client_node_name'] = 'true'
default['pushy']['opscode-pushy-server']['enable_graphite'] = false
default['pushy']['opscode-pushy-server']['graphite_host'] = 'localhost'
default['pushy']['opscode-pushy-server']['graphite_port'] = '2003'
default['pushy']['opscode-pushy-server']['keyring_dir'] = '/etc/opscode-push-jobs-server'

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
