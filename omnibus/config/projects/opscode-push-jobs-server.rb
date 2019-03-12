#
# Copyright 2012-2014 Chef Software, Inc.
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

name       "opscode-push-jobs-server"
maintainer "Chef Software, Inc. <maintainers@chef.io>"
homepage   "http://www.getchef.com"

license "Apache-2.0"
license_file "../LICENSE"

install_dir    "/opt/opscode-push-jobs-server"
build_version IO.read(File.expand_path("../../../../VERSION", __FILE__)).strip
build_iteration 1

# RPM spec files and DEB control files use slightly different formats
if ubuntu?
  runtime_dependency 'chef-server-core (>= 12.14.0)'
elsif rhel?
  runtime_dependency 'chef-server-core >= 12.14.0'
end

overrides_path = File.expand_path("../../../../omnibus_overrides.rb", __FILE__)
instance_eval(IO.read(overrides_path), overrides_path)

override :"chef-gem", :version => "14.8.12"
override :ruby, :version => "2.5.0"
# creates required build directories
dependency "preparation"

# global
dependency "libffi"
# These two are required for reconfigure, we don't actually need our
# own postgres server
dependency "runit"
dependency "postgresql"
dependency "pg-gem"

dependency "pushy-server-cookbooks"
dependency "opscode-pushy-server-ctl"

dependency "opscode-pushy-server"
dependency "pushy-server-schema"
dependency "oc-pushy-pedant"

dependency "chef"
dependency "ohai"

package :rpm do
  signing_passphrase ENV['OMNIBUS_RPM_SIGNING_PASSPHRASE']
end
