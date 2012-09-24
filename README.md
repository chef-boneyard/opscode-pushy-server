Overview
========

This is the oc-pushy-pedant repository; it is built on top of the chef-pedant repositories (core for OSC and OPC) to leverage the default accounts and so forth set up there.

Building
========

This repository currently requires a .deb build from the pushy branch of opscode-omnibus; it also requires that zeromq (version 2, not 3) be built before it's loaded.  This should only be a temporary requirement until the zeromq lib is added to opscode-omnibus.

1. Using the pushy .deb, build the OPC dev VM with 'rake start'.

2. Grab the v2 POSIX tarball from the [ZeroMQ site](http://www.zeromq.org/intro:get-the-software).  *No, seriously, download v2, not v3.*

3. In your VM, build zeromq.  Building it easy (after scp'ing it to the /tmp dir or wherever is convenient on the dev-vm):

        $ tar xvzf zeromq-<whatever-the-current-version-is>.tar.gz
        $ cd <unpacked dir>
        $ ./configure
        $ make
        $ make install

4. Load pushy server

        rake project:load[pushy]

5. Clone the proper repos (from above opscode-dev-vm):

        git clone git@github.com:opscode/oc-pushy-pedant.git
        git clone git@github.com:opscode/oc-pushy-pedant-tests.git
        git clone git@github.com:opscode/chef-pedant-core.git
        git clone git@github.com:opscode/oc-chef-pedant-core.git
        git clone git@github.com:opscode/opscode-pushy-client.git

6. Load pushy pedant (make sure you have the oc-pushy-pedant, oc-pushy-pedant-tests, chef-pedant-core, oc-chef-pedant-core and opscode-pushy-client repos checked out!!1!1!)

        rake project:load[oc-pushy-pedant]

7. Run tests:

        cd /srv/piab/mounts/oc-pushy-pedant
        ./oc-pushy-pedant -c multitenant_config.rb
