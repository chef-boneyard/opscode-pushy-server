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

* `opscode-omnibus` => OC-18/pushy
* `mixlib-authorization` => OC-18/pushy
* `pushy` => master (cut a feature branch)

## Build or steal an OPC deb

This will ensure you are developing against an OPC with Erlang R15B01 (you will
thank me later).

Decision time.  You can either:

1. Build your own artisanal OPC deb using `opscode-omnibus`:

        $ cd ~/oc/opscode-omnibus
        $ vagrant omnibus build ubuntu-10.04 private-chef

2. Grab a recent Pushy-aware deb from the seabuild1 ReadyNAS:

        $ cd ~/oc/opscode-omnibus/pkg
        $ wget https://readynas1.seabuild1.opscode.com/dev-general/opscode-east/opscode-omnibus/pkg/private-chef_1.1.19-133-geb2e9be-1.ubuntu.10.04_amd64.deb

## Stand up a dev-vm

We'll use the deb that was created/leached in the last step to stand up a dev-vm:

    $ cd ~/oc/opscode-dev-vm
    $ export OPC_INSTALLER=~/oc/opscode-omnibus/pkg/private_chef_X.X.X-X.ubuntu.10.04_amd64.deb
    $ rake start # choose 'Private Chef' environment

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
    $ bin/pushy-client -v -n DERPY

    # If you need to point to a different configuration endpoint:
    $ bin/pushy-client -v -s http://33.33.33.10:10003 -n DERPY

Feel free to start multiple clients..just be sure to give them all a
different name.

## Create (and execute) a Job

These commands should be entered in the console of the running pushy server.

First, insert a job record into the database:

    %% ensure the console knows about our records
    rr("/srv/piab/mounts/pushy/rel/pushy/lib/pushy/include/pushy_sql.hrl").
    %% list of nodes that should run job
    NodeNames = [<<"DERPY">>,<<"RAINBOWDASH">>].
    %% create an embroyonic job record
    Job = pushy_object:new_record(pushy_job, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, NodeNames).
    %% set the command and a max duration of 20 seconds
    Job1 = Job#pushy_job{command= <<"ohai">>, duration= 300}.
    %% insert job into the DB
    pushy_object:create_object(create_job, Job1, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>).

Now execute said job:

    %% register and execute the job
    pushy_job_runner_sup:execute(Job1#pushy_job.id).
