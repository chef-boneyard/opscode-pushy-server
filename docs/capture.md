Pushy Capture
========================

**Note that there is a bug in Mixlib::ShellOut, which causes some of
the tests to fail.  It was noted in CHEF-5294 (specifically, that the
Unix module's "reap_errant_child" function does sometimes tries to kill
a 'nil' process), and appears to still exist as of Sep 8, 2014.**

It is possible to capture the output of commands invoked by pushy,
as well as the stderr, by providing the "capture_output=true" option
when creating a job.  The resulting output is available through
a pair of new URIs:

    .../jobs/[job_id]/output/[node]/<stdout|stderr>

If the output is not available, querying the URL will return a 404.

Capture can be enabled on a per-job basis, meaning that it will affect all
nodes that run the job.  It is not possible to enable capture on a per-node
basis.

Enabling Capture
----------------
To enable capture, a job request may specify the attribute:

`capture_output`: *boolean*

By default, the value is `false`.  Querying the status of an existing
job, either via the job-id URL, or via the organization jobs URL,
will omit "capture_output" if its value is false.

Capture Retrieval
-----------------
To retrieve the stdout of a command, GET the URI:

    https://[server]/organizations/[org]/pushy/jobs/[job_id]/output/[node]/stdout

To retrieve the stderr, GET:

    https://[server]/organizations/[org]/pushy/jobs/[job_id]/output/[node]/stderr

The usual Chef authentication headers must be provided.

In both cases, Content-Type of the URI is `application/octet-stream`.

Capture Semantics
-----------------
If capture is enabled, then the client will capture both the stdout and
the stderr channels of the command.  The channel output will be sent back
regardless of whether the command succeeded or failed.  It will be sent
in raw form, including any terminating whitespace.  If the command produced
no output, then the value will be an empty string.

The two channels are treated as a pair -- for a given `<job, node>`, either
both will appear, or neither will appear.

The output is not streamed to the server as it is produced.  Therefore, the
output data for a given node will not be available until the run on that node
is complete.

Implementation Details
----------------------
### Data Storage
Both channels of the output are stored in a single row in the job_output table,
which uses the `<job_id, node_name>` as a primary key.  Both stdout and stderr
are NOT NULL, so if there is a bug in the client, and only one channel is
sent back, an error will be logged but no data will be stored in the table.

If the job is deleted, the DELETE will cascade to remove the job_output rows
for that job as well.

There is no expiration of the job output.  It may be appropriate to occasionally
delete old data, e.g.

    DELETE FROM job_output WHERE job_id IN
        (SELECT id FROM jobs WHERE updated_at < now() - interval '7 days')

### Capture happens even if capture_output is false
If the pushy-server receives capture information from the client, it will store
the result even if the job's "capture_output" flag is false (as long as *both*
stdout and stderr are present). In case of a buggy client, this provides
additional robustness; it also may make certain debugging scenarios easier.

### Commands are not terminated on abort
Because the implementation of pushy-client now uses the `Mixlib::ShellOut`
class to capture output, it also is limited by the restrictions of that class.
In particular, there is no method to kill a running process.  Therefore,
the previous semantics, in which an "abort" caused any running job to
end immediately, no longer works.  The client will still proceed with the
any future commands, but the process itself may run for up to 24 hours.
(The 24 hour limit is due to a hard-coded limit on any command invoked by
pushy-client.)


