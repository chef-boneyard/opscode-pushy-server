Overview
========

This is the oc-pushy-pedant repository; it is built on top of the chef-pedant repositories (core for OSC and OPC) to leverage the default accounts and so forth set up there.

Building
========

This repository currently requires a .deb build from the pushy branch of opscode-omnibus; it also requires that zeromq (version 2, not 3) be built before it's loaded.  This should only be a temporary requirement until the zeromq lib is added to opscode-omnibus.

1. Using the pushy .deb, build the OPC dev VM with 'rake start'.

2. Grab the v2 POSIX tarball from the [ZeroMQ site](http://www.zeromq.org/intro:get-the-software).  *No, seriously, download v2, not v3.*  For example, if it's 2.2, you can wget this:

        wget http://download.zeromq.org/zeromq-2.2.0.tar.gz

3. In your VM, build zeromq.  Building it easy (after scp'ing it to the /tmp dir or wherever is convenient on the dev-vm):

        tar xvzf zeromq-2.2.0.tar.gz
        cd zeromq-2.2.0
        ./configure
        make
        make install

4. Install the pushy server deb. It's easiest to grab a copy from jenkins.

        Browse to
        http://ci.opscode.us/job/opscode-push-jobs-server-build/build_os=ubuntu-10-04,machine_architecture=x64,role=opc-builder/
        to discover the latest version of the deb, then grab it and
        install it.

        wget http://ci.opscode.us/job/opscode-push-jobs-server-build/build_os=ubuntu-10-04,machine_architecture=x64,role=opc-builder/opscode-push-jobs-server_0.0.1-37-g40fc888-1.ubuntu.10.04_amd64.deb

        sudo dpkg -i opscode-push-jobs-server_0.0.1-37-g40fc888-1.ubuntu.10.04_amd64.deb

5. Load pushy server

        rake project:load[opscode-pushy-server]

6. Locally, clone the test repos (from above opscode-dev-vm):

        git clone git@github.com:opscode/oc-pushy-pedant.git
        git clone git@github.com:opscode/opscode-pushy-client.git

7. Load pushy pedant (make sure you have the oc-pushy-pedant, oc-pushy-pedant-tests, chef-pedant-core, oc-chef-pedant-core and opscode-pushy-client repos checked out!!1!1!)

        rake project:load[oc-pushy-pedant]

8. [OPTIONAL] Increase the heartbeat interval so the tests go faster

        vim /etc/opscode/private-chef.rb
        Add the line opscode_pushy['heartbeat_interval'] = 200
        private-chef-ctl reconfigure

9. In the VM, run the tests:

        cd /srv/piab/mounts/oc-pushy-pedant
        ./oc-pushy-pedant -c multitenant_config.rb


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

