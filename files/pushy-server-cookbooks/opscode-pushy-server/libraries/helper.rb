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

require 'chef/shell_out'

class OmnibusHelper
  def self.should_notify?(service_name)
    File.symlink?("/opt/opscode-push-jobs-server/service/#{service_name}") && check_status(service_name)
  end

  def self.check_status(service_name)
    o = Chef::ShellOut.new("/opt/opscode-push-jobs-server/bin/opscode-push-jobs-server-ctl status #{service_name}")
    o.run_command
    o.exitstatus == 0 ? true : false
  end

end

