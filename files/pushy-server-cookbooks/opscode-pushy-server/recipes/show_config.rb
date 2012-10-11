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

if File.exists?("/etc/opscode/opscode-pushy-jobs-server.rb")
  PushJobsServer[:node] = node
  PushJobsServer.from_file("/etc/opscode/opscode-push-jobs-server.rb")
end
config = PushJobsServer.generate_config(node['fqdn'])

puts Chef::JSONCompat.to_json_pretty(config)
