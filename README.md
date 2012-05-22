Overview
========

"pushy" is the internal nickname for the new push job feature. In this repo you
will find:

* A org-mode file describing the overall design
* A spec compliant agent suitable for production deployment
* A spec compliant server suitable for inclusion in a production OPC installation.

Run ALL THE THINGS
==================

## Load the schema

Ensure mixlib-authorization is on the 'pushy' branch.

    $ cd ~/oc/mixlib-authorization
    $ git checkout pushy

Load mixlib-authorization into the dev-vm:

    $ cd ~/oc/opscode-dev-vm
    $ rake project:load[mixlib-authorization]

SSH into the dev-vm and run Sequel migrations:

    $ rake ssh
    vagrant@private-chef:~$ cd /srv/piab/mounts/mixlib-authorization
    vagrant@private-chef:/srv/piab/mounts/mixlib-authorization$ bundle exec sequel -m db/migrate postgres://opscode-pgsql@127.0.0.1/opscode_chef

## Start the server

Load pushy/server into the dev-vm:

    $ cd ~/oc/opscode-dev-vm
    $ rake project:load[pushy]

Start the party:

    $ rake ssh
    vagrant@private-chef:~$ cd /srv/piab/mounts/pushy
    vagrant@private-chef:/srv/piab/mounts/pushy$ ./rel/pushy/bin/pushy console

## Start a client

Start a client on your host:

    $ cd ~/oc/pushy/client
    $ bundle install
    $ ./bin/pushy-client -v -n DERPY

Feel free to start multiple clients..just be sure to give them all a
different name.
