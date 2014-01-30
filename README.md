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
        $ wget https://readynas1.seabuild1.opscode.com/dev-general/opscode-east/opscode-omnibus/pkg/private-chef_1.2.1-49-g1acbb36-1.ubuntu.10.04_amd64.deb

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

Before you can start a client, install zeromq.

    OS X: brew install zeromq

Start a client on your host:

    $ cd ~/oc/opscode-pushy-client
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
    Job1 = Job#pushy_job{command= <<"ohai">>, run_timeout= 300}.
    %% insert job into the DB
    pushy_object:create_object(create_job, Job1, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>).

Now execute said job:

    %% register and execute the job
    pushy_job_runner_sup:execute(Job1#pushy_job.id).

Measure ALL THE THINGS
======================

Pushy uses [Folsom](https://github.com/boundary/folsom) to collects metrics.  The
list of current metrics are as follows:

| Pushy Event                                                  | Metric Types Caputures |
|:-------------------------------------------------------------|:-----------------------|
| Send message generate signature                              | Counter, Histogram     |
| Send message total processing time (includes sig generation) | Counter, Histogram     |
| Receive message signature verification                       | Counter, Histogram     |
| Receive message total processing time (includes sig verify)  | Counter, Histogram     |

Pushy currently does not send it's metrics to any sort of upstream collector (ie
estatsd and graphite).  An Erlang console session is the easiest way to retrieve
the metrics.

    %% list all available metrics
    (pushy@127.0.0.1)2> folsom_metrics:get_metrics().
    [<<"receive.verify_sig.histogram">>,<<"send.all.counter">>,
     <<"receive.all.histogram">>,<<"send.gen_sig.counter">>,
     <<"receive.verify_sig.counter">>,
     <<"send.gen_sig.histogram">>,<<"receive.all.counter">>,
     <<"send.all.histogram">>]

    %% list current value of counter metric
    (pushy@127.0.0.1)3> folsom_metrics:get_metric_value(<<"receive.all.counter">>).
    98

    %% processed histogram
    (pushy@127.0.0.1)4> folsom_metrics:get_histogram_statistics(<<"receive.all.histogram">>).
    [{min,411},
     {max,88473},
     {arithmetic_mean,1279.8984375},
     {geometric_mean,946.0903396041191},
     {harmonic_mean,924.2236997743212},
     {median,928},
     {variance,29948563.95827206},
     {standard_deviation,5472.528113977311},
     {skewness,15.799144482448193},
     {kurtosis,248.73093069189437},
     {percentile,[{75,976},{95,1146},{99,1534},{999,88473}]},
     {histogram,[{3511,255},
                 {7411,0},
                 {10411,0},
                 {13411,0},
                 {16411,0},
                 {19411,0},
                 {22411,0},
                 {25411,0},
                 {28411,0},
                 {31411,0},
                 {40411,0},
                 {50411,0},
                 {60411,0},
                 {70411,...},
                 {...}|...]}]

    %% raw histogram data
    (pushy@127.0.0.1)5> folsom_metrics:get_metric_value(<<"receive.all.histogram">>).
    [917,880,965,833,962,978,960,864,899,891,928,945,970,962,
     966,942,995,935,904,902,852,849,995,992,931,905,963,897,979|...]


Logging with Lager
=================

Logging in Pushy is done using [Lager](https://github.com/basho/lager).

We define 3 log files located in /var/log/opscode/opscode-pushy:

* console.log
    * contains all log messages from the "info" level and above.
* error.log
    * contains all log messages from the "error" level and above.
* jobs.log
    * contains logs specific to jobs.

By default the pushy console will display "info" level messages and above. The
log level can be changed on the fly with lager. The log level can be set to any
of: debug, info, notice, warning, error, critical, alert, emergency.

    (pushy@127.0.0.1)1> lager:set_loglevel(lager_console_backend, debug).


Database Migration hack
=======================
Moved database migrations into db
Run them under dev-vm as
% bundle exec sequel --echo -m migrate postgres://opscode-pgsql@127.0.0.1/opscode_pushy
TODO
* Very likely will have to create the database; look at
   opscode-dev-vm/cookbooks/piab/recipes/dev.rb and the create dev db
* Also need changes to the recipe doing the pushy config to make sure we're
  actually pointing at the opscode_pushy db.


License
=======

Pushy - The push jobs component for chef

|                      |                                          |
|:---------------------|:-----------------------------------------|
| **Copyright:**       | Copyright (c) 2008-2014 Chef Software, Inc.
| **License:**         | Apache License, Version 2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
