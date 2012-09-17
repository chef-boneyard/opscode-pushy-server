Overview
========

This is the oc-pushy-pedant repository; it is built on top of the chef-pedant repositories (core for OSC and OPC) to leverage the default accounts and so forth set up there.

Building
========

This repository currently requires a .deb build from the pushy branch of opscode-omnibus; it also requires that zeromq (version 2, not 3) be built before it's loaded.  This should only be a temporary requirement until the zeromq lib is added to opscode-omnibus.

1. Using the pushy .deb, build the OPC dev VM with 'rake start'.

2. Grab the v2 POSIX tarball from:

        http://www.zeromq.org/intro:get-the-software

3. Build zeromq.  Building it easy (after scp'ing it to the /tmp dir or wherever is convenient on the dev-vm):

        $ tar xvzf zeromq-<whatever-the-current-version-is>.tar.gz
        $ cd <unpacked dir>
        $ ./configure
        $ make
        $ make install

4. Load pushy server

        rake project:load[pushy]

5. Load pushy pedant

        rake project:load[oc-pushy-pedant]

6. Run tests (in /srv/piab/mounts/oc-pushy-pedant):

        ./oc-pushy-pedant -c mulitenant_config.rb
