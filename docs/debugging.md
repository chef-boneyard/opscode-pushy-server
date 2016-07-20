# Debugging Tips

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

Create (and execute) a Job
==================

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
