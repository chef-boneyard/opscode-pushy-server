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

name "opscode-pushy-server"
version "1.1.0"

dependency "erlang"
dependency "rebar"
dependency "rsync"
dependency "curl"
dependency "automake"
dependency "autoconf"
dependency "libuuid"
dependency "libtool"
dependency "bundler"

# TODO - use public GIT URL when repo made public
source :git => "git@github.com:opscode/opscode-pushy-server.git"

relative_path "opscode-pushy-server"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CXXFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-pushy-server"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./rel/opscode-pushy-server/ #{install_dir}/embedded/service/opscode-pushy-server/"
  command "rm -rf #{install_dir}/embedded/service/opscode-pushy-server/log"
end
