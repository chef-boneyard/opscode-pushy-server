Overview
========

This is the oc-pushy-pedant repository; it is built on top of the
chef-pedant repositories (core for OSC and OPC) to leverage the
default accounts and so forth set up there.

Building
========

1. Start a chef-server with `vagrant up` in the `dev/` folder of
   chef-server.

1. Log into your dev-vm: `vagrant ssh`

1. Using the pushy .deb you wish to test, install pushy:

        dpkg -i /path/to/package
        opscode-push-jobs-server-ctl reconfigure
        chef-server-ctl reconfigure

1. Clone oc-pushy-pedant inside your dev-vm

        git clone git@github.com:opscode/oc-pushy-pedant.git

1. [OPTIONAL] Increase the heartbeat interval so the tests go faster

        vim /etc/opscode-push-jobs-server/opscode-push-jobs-server.rb
        # Add the line: opscode_pushy_server['heartbeat_interval'] = 1000
        opscode-push-jobs-server-ctl reconfigure

1. In the VM, run the tests:

        export LD_LIBRARY_LOAD=/opt/opscode-push-jobs-server/embedded/lib
        bundle install
        ./oc-pushy-pedant -c /etc/opscode-push-jobs-server/pedant_config.rb


License
=======

Pushy - The push jobs component for chef

|                      |                                          |
|:---------------------|:-----------------------------------------|
| **Copyright:**       | Copyright (c) 2008-2014 Chef Software, Inc.
| **License:**         | Apache License, Version 2.0

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
