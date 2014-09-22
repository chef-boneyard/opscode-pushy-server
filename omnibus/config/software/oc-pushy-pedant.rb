#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "oc-pushy-pedant"
default_version "1.0.6"

dependency "libzmq"
dependency "ruby"
dependency "bundler"

# TODO: use the public git:// uri once this repo is public
source git: "git@github.com:opscode/oc-pushy-pedant"

relative_path "oc-pushy-pedant"

build do
  bundle "install" \
         " --path=#{install_dir}/embedded/service/gem"
  sync "#{project_dir}", "#{install_dir}/embedded/service/oc-pushy-pedant/"
end
