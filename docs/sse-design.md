Pushy Server-Sent Events
========================

The pushy-server provides feeds of events associated with actions, via the
"Server-Sent-Events" protocol (http://www.w3.org/TR/eventsource/).  There are
two different feeds, one for the events for a particular job, one for every job
across an organization.  For jobs, a client may open an SSE request to the
server, and receive events for the job start, node votes, node run completion,
and job completion.  For an organization, the client may request events for job
starts and completions.  The events don't provide any summary information, but
the goal is for the events to provide enough information that smart clients
can calculate statistics about job progress, failure rates, etc.

Server-Side Events
------------------
As mandated by the SSE protocol, each event has a type-specifier, an ID, and
data.  The event type is simply a string, that can be used for dispatching the
event.  The ID may be used in an optional "Last-Event-ID" header in the
request, to request all the events since a particular event was received.  The
data format is unspecified by the SSE spec; in pushy's case it is a JSON
string containing a single object.  The specific fields in the object are
different for each event type, and are specified below.

In addition, SSE allows comments in the stream, indicated by a starting colon.
Pushy-server uses comments to send "no-op" events every 15 (configurable)
seconds, as a form of keepalive for the socket.

The SSE spec suggests that HTTP chunking not be used for events, but the
current implementation of pushy-server does in fact use chunking.  See the
"Problems" section below.

API
---
The SSE feed is restricted by the usual constraints on client requests: the
request must be signed with a valid authentication header, and the associated
user must be in the pushy_jobs_readers group.

When the request is made, a series of past events may be provided, and then
the stream will block until new events are available.  The client can control
how much of the historical stream is provided -- see "Last-Event-ID" below.

For jobs, it is possible to ask for all the events since the job started.  Once
the job completes, the stream will also complete and the server will close its
side of the HTTP connection.

For organizations, old events expire after some period, so only the most recent
events can be retrieved.  In normal operating circumstances, the stream of
events from an organization never completes.

### URLs
For jobs, the feed is available by doing a GET request on:

    https://[server]/organizations/[org]/pushy/job_status_feed/[job_id]

For organizations, the feed is available from a similar path, without the
[job_id]:

    https://[server]/organizations/[org]/pushy/job_status_feed

### Accept Header
The request must specify the header:

    Accept: text/event-stream

### Last-Event-ID
As specified in the SSE protocol, the client may request all the events since
a particular ID, by including the optional header:

    Last-Event-ID: [ID]

For jobs, if the header is not included, or the ID is not recognized, then the
stream will start from the beginning of the job.

For organizations, if the header is not included, no events will be produced
until the next activity in the organization produces an event.  If a
Last-Event-ID is passed but not recognized, it is assumed that the ID
has already expired, and the feed will produce the events as far back as are
recorded, preceded by a synthetic event, of type `start_of_history`.

The expiration time of the organization events is 60 seconds (configurable).

### Completed jobs
In the job feed, for a brief period after a job completes, the event stream
will remain available.  The request will behave as normal, which means the
client will see the stream of events from the beginning of the job (or if
specified, the Last-Event-ID), through to the job completion, and then the
connection will be immediately closed by the server.  The amount of time before
it completes is configurable, but defaults to 5 seconds.

If a request is made for the feed after the waiting period, then the request
will result in a single event, a "summary" event, which contains the same data
as requesting a named-job resource.  In this case, any Last-Event-ID will be
ignored.

### Multiple simultaneous clients
Multiple clients may request events at the same time.  They will all receive
events individually, as normal.

Events
------
### Event Format
Each event has the same format:

    event: [name]
    id: [ID]
    data: [JSON object]

Note that the IDs are not guaranteed to be globally unique. Instead, job event
ids are unique within the stream for a particular job, and organization event
ids are unique within the stream for a particular organization.  (Put another
way, event IDs are unique for the URL that produces them.)  IDs should be
treated as opaque strings.

The contents of the JSON object depend on the event, but each event includes a
(server-based) timestamp, in ISO8601 format, including a (not particularly
reliable) microsecond offset.  For example:

    2014-07-10 05:17:44.995958Z

Most of the events below are potentially produced on a job feed.  On the
organization feed, only the `start` and `job_complete` events are produced.
A given job-start and job-completion and will produce the same event on both
feeds, with two differences:

* The ids may be different
* The organization event will include the an additional `job` attribute, which
  specifies the job-id.

### start
This is issued when the job is requested.

* command: the command being run
* run_timeout: the timeout in seconds specified in the job request
* quorum: the number of clients to accept the command, as specified in the
    job request
* user: the user making the job request
* node_count: the number of nodes in the request
* job (only in organization feeds): the id of the job

Note that the list of nodes is not provided.  A subscriber can determine the
nodes by observing the stream of responses, although this is not 100% reliable
because a slow or offline node may not respond before the run is complete
(which is possible when the quorum is less than 100%).  But the set of nodes
is always available in the summary event, or in the named-job request.

### quorum_vote
This is issued as each node responds to the quorum request.

* node: the node responding to the vote
* status: "success" on success, anything else on failure.  The details of
    the status may provide more information about why it failed.  E.g.
    "failure" means the node responded with a nack.  "lost_availibility" or
    "client_died_while_voting" mean there was a problem with the node, etc.
    See the source for details.

(not produced by organization feed)

### quorum_succeeded
This is issued when the vote is complete, and the nodes are told to run the
command.  There is no further information in the event, aside from the
timestamp.

Note that there is no corresponding "quorum_failed" event -- if the quorum
fails, the job_complete event will include a "quorum_failed" status.

(not produced by organization feed)

### run_start
This is issued as each node acks that it is beginning the run.  This should
normally occur immediately after a quorum_succeeded.  The timestamp may be
different, however, particularly if there are a large number of clients.  This
event allows for a more accurate determination of the time required for each
node to run the command.

* node: the node running the command

(not produced by organization feed)

### run_complete
This is issued when each node completes the command.

* node: the node completing the command
* status: "success" if the command succeeded, anything else on failure.
    The details of the status may provide more information about why it
    failed.  E.g.  "crashed" means the node crashed.
    "run_nacked_while_running" means the (buggy) client sent a "run_nack"
    after it had already sent a "run_ack".  Etc.  See the source for details.

(not produced by organization feed)

### job_complete
This is issued when the job completes.

* status: "complete" for normal completion, anything else for failure.
    E.g. "quorum_failed" if the quorum failed, "timed_out" if the job's
    "run_timeout" was reached before all the clients completed their runs,
    etc.  See the source for details.
* job (only in organization feeds): the id of the job

### start_of_history
This is issued when the Last-Event-ID is not recognized in an organization
feed request.  If this event appears, then there is a possibility of a gap in
the event stream seen by the client.

Note that the ID in this event is synthetic -- multiple requests to the feed
may produce the same ID for this event, although the timestamps will be
different.

The timestamp contains the expiration horizon time.  I.e. by default it
will specify a time about 60 seconds before the request was made.

There are no other attributes in this event.

(not produced by job feed)

### rehab
This is issued any time the server sends a node to rehab (i.e. sends an
"abort" command to the node, to reset it to a known state.)  This should not
happen during normal runs, and is generally independent of the state of the
job, but it may be useful for monitoring and debugging.

* node: the node being rehabed

(not produced by organization feed)

### summary
This is issued only when a request for the event feed comes in after the job
has completed.  The data is exactly the same data as in the result of a
named-job request.

(not produced by organization feed)

Job event stream
------------
An event stream should follow a standard structure:

### Normal run completion

* start
* 0+ quorum_vote
* quorum_succeeded
* 1+ run_start
* 1+ run_complete
* job_complete

Note that the run_start and run_complete events may be interleaved for
different nodes.

If the job times out, there may be fewer run_complete events than run_start
events.

### Failed quorum

* start
* 1+ quorum_vote
* job_complete

### Others
In general, anything other than the above will occur because a node had a
problem, in which case there will be one or more rehabs in the middle.

Organization event stream
------------
An organization event stream follows a much simpler structure:

* start (job=[jod-id])
* job_complete (job=[jod-id])

Note that multiple jobs may be interleaved; e.g. the stream may look like:

* start (job=1)
* start (job=2)
* job_complete (job=2)
* job_complete (job=1)

Also, the feed may be requested after a job has already started, in which
case there may be no event for the the start, but only for the completion.

Configuration
-------------
The following configuration variables control the pushy-server behavior:

* wait_complete_time (integer, default 5): the number of seconds that a
    job waits around after it completes, allowing it to provide a full event
    stream instead of a summary.
* keep_alive_time (integer, default 15): the number of seconds between
    keepalive messages being sent on the event stream.
* org_feed_expiration (integer, default 60): the number of seconds before
    an organization event expires.

Design Notes
------------
### No summary info
As noted above, there is no summary information in the job events (e.g. as the
votes are coming in, providing a summary such as "3 success, 2 failure, 5 to
go").  This is because the information is available from the event stream.  The
decision was made to avoid providing redundant information in the event stream,
instead requiring the clients to accumulate and summarize the information as
appropriate.

### Not using the named-job endpoint
It would be possible to provide the feed on the normal named-job URL endpoint,
by distinguishing the two interfaces with the Accept header (text/event-stream
vs. application/json).  But that would make it harder to configure nginx
differently for live streams vs. static results, or for switching the event
implementation to, say, cowboy.

### No nodes in the start event
The start event contains most of the information in the job request; however,
it leaves out the list of nodes.  This is again for the sake of minimalism --
in the normal case, the information is derivable from the rest of the event
stream.  However, since there are situations in which the information is not
available (at least, not until the job completes) it may make sense to add
this information in the future, if the need arises.

Problems
--------
The event stream is built, like the rest of pushy-server, on the webmachine
framework.  However, webmachine is specifically designed for REST-ish
use-cases, and explicitly documents that it is not ideal for live streaming of
data.  It works adequately, but there is a problem that is impossible to
fix, without either hacking on webmachine, or switching to another framework
such as Cowboy:

### Chunking
The SSE document says:

> Authors are also cautioned that HTTP chunking can have unexpected negative
> effects on the reliability of this protocol. Where possible, chunking
> should be disabled for serving event streams unless the rate of messages
> is high enough for this not to matter.

Unfortunately, webmachine has only one mechanism of providing a live stream of
data, and that mechanism uses chunking in the HTTP protocol.  It is not clear
what problems this may cause yet.  If it needs to be fixed, the easiest
solution would be to fork webmachine, at which point the changes to the code
are fairly minimal.
