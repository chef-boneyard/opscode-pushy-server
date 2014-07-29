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

name       "opscode-push-jobs-server"
maintainer "Chef Software, Inc."
homepage   "http://www.getchef.com"

replaces        "opscode-push-jobs-server"
install_path    "/opt/opscode-push-jobs-server"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

override :berkshelf, version: "v2.0.15"

runtime_dependency "private-chef"

# creates required build directories
dependency "preparation"

# global
dependency "libffi"
dependency "chef-gem"
dependency "pushy-server-cookbooks"
dependency "pushy-server-scripts"
dependency "opscode-pushy-server-ctl"
dependency "runit"

dependency "opscode-pushy-server"
dependency "pushy-server-schema"
dependency "oc-pushy-pedant"

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
