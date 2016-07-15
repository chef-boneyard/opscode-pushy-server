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

name "opscode-pushy-server"
default_version "2.0.1"

license "Apache-2.0"
license_file "LICENSE"

source path: "#{project.files_path}/../../"

dependency "erlang"
dependency "rebar"
dependency "curl"
dependency "automake"
dependency "autoconf"
dependency "libuuid"
dependency "libtool"
dependency "bundler"

relative_path "opscode-pushy-server"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  # erlzmq2 needs to know where to find the embedded zeromq, or it tries to build its own.
  env['ZEROMQ_PREFIX']="#{install_dir}/embedded"
  make "distclean", env: env
  make "rel", env: env
  sync "#{project_dir}/_build/default/rel/opscode-pushy-server/", "#{install_dir}/embedded/service/opscode-pushy-server/"
  delete "#{install_dir}/embedded/service/opscode-pushy-server/log"
end
