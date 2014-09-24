#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#


name "opscode-pushy-server"
default_version "1.1.0"

source git: "git://opscode/opscode-pushy-server"

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
  make "distclean", env: env
  make "rel", env: env
  sync "#{project_dir}/rel/opscode-pushy-server/", "#{install_dir}/embedded/service/opscode-pushy-server/"
  delete "#{install_dir}/embedded/service/opscode-pushy-server/log"
end
