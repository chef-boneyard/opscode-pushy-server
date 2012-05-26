Overview
========

"pushy" is the internal nickname for the new push job feature. In this repo you
will find:

* A org-mode file describing the overall design
* A spec compliant agent suitable for production deployment
* A spec compliant server suitable for inclusion in a production OPC installation.

Run ALL THE THINGS
==================

## Ensure host-based checkouts of all projects are on the correct branches

* `opscode-omnibus` => pushy
* `mixlib-authorization` => pushy
* `pushy` => 54/integration

## Reconfigure OPC

Load `opscode-omnibus` and generate the artisanal OPC-specific app.config for pushy:

    $ cd ~/oc/opscode-dev-vm
    $ rake project:load[opscode-omnibus]
    $ rake update

## Load the new database schema

Load `mixlib-authorization` and migrate the database:

    $ cd ~/oc/opscode-dev-vm
    $ rake project:load[mixlib-authorization]
    $ rake migrate

## Load pushy into dev-vm

    $ cd ~/oc/opscode-dev-vm
    $ rake project:load[pushy]

## Start the pushy server

    $ cd ~/oc/opscode-dev-vm
    $ rake ssh
    vagrant@private-chef:~$ cd /srv/piab/mounts/pushy
    vagrant@private-chef:/srv/piab/mounts/pushy$ sudo ./rel/pushy/bin/pushy console

## Start a client

Start a client on your host:

    $ cd ~/oc/pushy/client
    $ bundle install
    $ ./bin/pushy-client -v --in-address tcp://33.33.33.10:10001 --out-address tcp://33.33.33.10:10000 -n DERPY

Feel free to start multiple clients..just be sure to give them all a
different name.
