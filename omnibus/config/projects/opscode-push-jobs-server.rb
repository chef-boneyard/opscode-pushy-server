#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name       "opscode-push-jobs-server"
maintainer "Chef Software, Inc."
homepage   "http://www.getchef.com"

replace         "opscode-push-jobs-server"
install_dir    "/opt/opscode-push-jobs-server"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

override :berkshelf, version: "v2.0.15"

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
