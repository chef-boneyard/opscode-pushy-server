#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "pushy-server-schema"
default_version "1.0.0"

# To install, we need sqitch, but we can use the one already installed
# for Enterprise Chef

source git: "git@github.com:opscode/pushy-server-schema.git"

build do
  sync project_dir, "#{install_dir}/embedded/service/#{name}/"
end
