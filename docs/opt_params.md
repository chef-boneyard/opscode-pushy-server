Pushy Optional Parameters
=========================

The pushy-server provides for various optional parameters controlling
the execution of a job on the various nodes.  These parameters allow
the job-invoker to specify the user to run as on the pushy-client
nodes, the directory to run the command from, additional environment
variables to set before invoking the command, and a file (up to
100K) to be transferred to the client before the command is run.

Along with the new parameters, there is a new environment variable,
PUSHY_JOB_ID, which is provided to the command when it runs.

New Parameters
--------------

### User: string
This is the name of the user to run the command as.  Both the real and
effective uids are changed to specified user.  If the user does not
exist on the client node, the run request will be rejected.

### Dir: string
This is the directory to run the command from.  It should be specified
as an absolute path.  If the directory does not exist on the client
node, the run request will be rejected.

### Env: key-value list
This list of key-value pairs will be directly added to the environment,
without interpretation.

### File: string -- "raw:..." or "base64:..."
This is a string that will be stored as a file on each node, with the
path being provided to the command as an environment variable.
Note that the string has a required prefix of "raw:" or "base64:",
indicating the encoding (if any) of the contents.  The user of the API
is trusted to make the choice to use Base64 when the contents of the string
could cause any problems with JSON or HTTP representation.  It would be
safe to always encode the file as Base64, but there may be basic uses of
the file parameter that don't require encoding, and in those situations,
debugging may be easier if the contents are human-readable.

The intended use of the file is to provide configuration information
for commands that may not fit on a command-line.

API Changes
-----------
All user-visible descriptions of jobs now include the optional parameters.
These include the job description when it is created, the results of querying
the pushy-server for the status of existing and completed jobs, and the
events in the event feed of a job or organization.

However, the contents of the job descriptions, and in particular the
representation of files, may be different in the different cases.

### Job creation (POST .../pushy/jobs)
The API is extended to include the four additional parameters above,
as optional attributes of the JSON object describing the job.  Specifically,
the new attributes are:

* "user": string
* "dir": string
* "env": object: { string : string, ...}
* "file": string

The additional requirements at the API level are that there be no duplicate
keys in the env, and that the file begin with one of the prefixes "raw:"
or "base64:".  If the prefix is "base64:", then the request will be rejected
if the remainder of the string is not a valid Base64 encoding.  It will
also be rejected if the file is > 100K.

### Job information (GET .../pushy/jobs/<job_id>)
The API is extended to include the user, dir, and env, if they were specified.
The "user", "dir", and "env" fields are as specified above.  They are all
optional.

Files are handled specially, since requests will often not be interested in
the contents of the files.  Therefore, by default, the job information
will only include a boolean indicating whether a file had been specified:

* "file_specified": boolean

(This parameter, as with the others, is optional.  It will only appear if
with the value "true".)

However, if the user wishes to get the file itself, the request may include
the query string: "?include_file=true".  If this query is present, then
the "file_specified" attribute will not be present; instead, the file
(prefixed with "raw:" or "base64:") will be provided.

### Jobs information (GET .../pushy/jobs)
If the list of jobs for an organization is requested, the job information
will be provided as for a single job, but there is no option to specify
that that file contents be provided.  All jobs with files will have a
"file_specified": "true" attribute.

### Event feed (GET .../pushy/job_status_feed...)
The same parameters are included in job- and org-level start events.
Again, the "file_specified" parameter will be used to indicate the
existence of a file.  There is no option to see the file contents
in the event feed.

Client Changes
--------------
The client now executes the command with the additional parameters
specified in the new options.  It also provides the job-id to the
command as part of the expanded environment.

Because the files need to be stored locally, there is a mechanism in
the client for cleaning up old files.  The directory used for storing
these temporary files is, by default, "/tmp/pushy", but there is now
a configuration option for specifying the location.

### New Parameters

#### user
When the user is specified in a job, it is checked at the "commit" stage
to determine whether the user exists.  If not, the job is rejected by
the client.

At the run stage, the client forks a new process, sets the real and effective
uids, then invokes the command.

#### dir
When the directory is specified in a job, it is checked for existence
at the "commit" stage.  if it doesn't exist, the job is rejected.

At the run state, the child process of the client chdirs to the specified
directory before invoking the command.

#### env
The client does no checking of the env at the commit stage.

At the run stage, the client runs the command with an augmented
environment that includes all the specified key-value pairs.

#### file
The client does no checking of the file contents (beyond a sanity
check that a valid file prefix is provided).

At the run stage, the string is extracted into a file, located in the
"file_dir", the directory configured to contain temporary job files.
When the command is executed, it will include an additional environment
variable: "PUSHY_JOB_FILE", which contains the absolute path of the
extracted file.  The file will remain in existence after the job completes,
for debugging purposes (but see below about clean-up).

### New environment variables
Along with the previously-implemented "PUSHY_NODE_NAME" variable, the
client now gets the variable "PUSHY_JOB_ID", which is a string containing
the current job id.  This string could be used for logging purposes, or
for as a unique identifier, since it is effectively guaranteed to be unique
across all jobs ever invoked.

In addition, the "PUSHY_JOB_FILE" variable will be set if a file was specified
in the job, as described above.

### New Configuration Options
The directory containing the temporary job files can be configured as follows:

* short option: -d DIR
* long option: --file_dir DIR
* default: /tmp/pushy

### Directory Cleanup
The temporary job files are not removed immediately upon job completion,
because there is some value in being able to inspect those files, for
debugging purposes.  Instead, the pushy_client will clean up files
older than 24 hours.  It does this purge every 24 hours (so files
may actually last up to two days, depending on the time in the expiration
cycle when the job is created).

Note that the temporary files will always be of the form ".../pushy_file...".
The cleanup process will ignore any files that do not have the "pushy_file"
prefix, in case someone accidentally configured the file_dir to be a
directory used by other systems, e.g. "/tmp".

The files are randomly-chosen -- there is no indication of a job-id in the
filename.  If debugging is required, it is recommended that commands which
read the PUSHY_JOB_FILE also log the path, so that a particular config
file can be easily identified.

Implementation Details
--------------------
Along with the above user-visible changes, changes have been made to the
postgres layer, and the 0mq protocol.

### Database Changes
The postgres database now contains a new table: "job_options", which uses
the job-id as a primary key, and contains NULLable columns for the four
optional parameters.  If all the options would be NULL, no row is written.
The query for retrieving job information now does a left outer join from the
job table to the job_options table.

#### Schema
The schema for the new table is the following:

    CREATE TABLE job_options (
        job_id character(32) NOT NULL,
        job_user character(32),
        dir character(256),
        env text,
        job_file text
    );

    ALTER TABLE ONLY job_options
        ADD CONSTRAINT job_options_fkey FOREIGN KEY (job_id) REFERENCES jobs(id) ON UPDATE CASCADE ON DELETE CASCADE;

Note that both the env and the job_file are unlimited in size, although
the file is limited by the pushy-server to 100K or less. The job_file
is stored in the same form as in the API, i.e. with a "raw:" or "base64:"
prefix indicating the encoding.

The env is the JSON-encoded version of the key-value object.
There is no limit on the env.  This follows the pattern of some other
columns in the DB, including the job command.  The only restriction
on these values is the 1M limit for any POST request coming in to the
pushy-server.

#### Migration
Because the new information is in a different table, there is no need to
migrate the existing data, since all the previous data is still in the
same format.  However, the pushy-server using the new queries will fail
when run against an unmodified database, due to the join against the new
table.  Therefore the schema must be updated before invoking the new
pushy-server code.  This can be as simple as invoking the above "CREATE
TABLE" and "ALTER TABLE" commands.

### Protocol changes
The 0mq "commit" request now contains the four parameters,
user, dir, env, and file.  These are additional, optional JSON attributes
in the commit message.

Implementation Notes
--------------------
### Things left out
There are various potential features that do not exist in the initial
implementation.  These should be revisited once there is some experience
from live users of the new features.

#### Multiple files
It is only possible to provide a single file, since that matches the
prototypical use case of an extended configuration string.  However,
one can easily imagine other uses for the file, in which having a
single file may be an unfortunate limitation.  Adding multiple files
should not be difficult.  However, some questions immediately arise:

* If a command is intended to make use of multiple files, how does it
know which is which?  Should the file specification be a name->path
mapping?  How is this represented when invoking the command?
* What should the limits be, on the number of files, the size of
individual files, the size of the aggregate set of files, etc?

#### Command-line expansion
Instead of specifying job-specific command information via environment
variables, it may be more natural to use command-line parameters.
The command line could include values, e.g. '$JOB_ID', which would
be expanded by the client when invoking the command.
However, that effectively means the command line becomes a small DSL,
with a set of questions, e.g.:

* What should the expansion syntax be?  Can expansion be suppressed?
Is there a way to doubly-expand variables?
* How are job-specific parameters passed to the client?  Is there another
key-value list like "env", that is just used for command-line expansion?
* What other system parameters should be defined?  USER?  CWD?

#### File compression
There may be some value in compressing the files for transport and/or
database storage.  But adding that compression at the moment would be
premature optimization.

#### Clearing the environment
Environment variables may currently be added, but there is no provision
for clearing/removing an environment variable.  There are two obvious
approaches to this feature: to clear the entire environment before adding
new values, or to clear specific values.

However, each brings up questions, primarily around the safety and semantics
when critical environment variables such as PATH and SHELL are involved.
In addition, it is not clear what value this feature would provide,
and whether having the client clear the variables (as opposed to the command
itself) is the best solution.  This feature needs a driving use-case to
guide the design.

#### Job-ID in the filename
This may be a useful addition, to simplify finding the file associated with
a job when debugging.  However, since job-ids are already kind of long,
it seems like the resulting file-names could get annoyingly cluttered.
But the main reason is that no one considered this issue until the
implementation was complete.  It is worth considering in the future.

#### Files in a separate 0mq frame
It would be more efficient to decode the file when it arrives in the
server, into a raw format (e.g. when encoded as Base64), and stored
and transported in this raw format, since Base64 adds a 33% overhead to
ths size of any string.  However, to avoid issues with HTTP and JSON
transport, the file is always stored in the "safe" format in which
it was originally provided.  (The system trusts that the user creating
the job is making a correct choice about whether the chosen encoding is
safe.)

The server could instead track the file as raw bytes, and only convert
it to base64 when/if that is necessary.  In particular, since the file
may be sent many times when a job is run (once for each node),
there might be value in sending the file in raw form.  Since raw form
may not play well with JSON, the natural choice would be to send it as a
separate frame in the 0mq message.

However, the 0mq protocol is very consistently implemented on the client
and the server as always having a particular frame structure, with the
main "body" frame containing the JSON representation of the message.
Changing this on the client and the server would be a significant chunk
of additional work.  So again, it sits in the category of "premature
optimization".

#### Configuration of the file_dir cleanup period
Right now the 24-hour period for file_dir clean-up is hard-coded.  It might
be nice to allow it to be configurable on the command line.  This would be
easy to add if the need arose.

#### File commit/quorum/run logic
Currently the file is sent at the commit stage.  However, if the
job does not pass the quorum, it will never be run, and the fact
that the data was sent is inefficient.  (This is of course true of
any information associated with the command, but the file in particular
is assumed to be dominatingly-large when it exists.)

There are two reasons not to make this change.  The first is that
the system generally has an optimistic assumption: jobs that are
requested will usually be run.  Jobs that are not run are usually
a sign of an exceptional circumstance.  Therefore, assuming the job will
be run suggests that the file could be sent in the commit msg just as
well as in the run msg.

The other reason is that, the earlier the file is sent, the earlier
there is a possibility for the client to reject the command.  In particular,
one could imagine situations in which a run could be rejected based on
the file contents.  For instance, there might not be enough disk space to
store the file.  Or conceivably the client could change to let the
command itself validate the file contents, to ensure that they make sense.
These changes would both move the pushy model closer to a transactional
model.  They require that the file be sent at the commit stage.

However, the above checks are not implemented, so it is clearly the
case that the file could be sent at the run stage given the current
semantics of the client.  So this change should remain under consideration.

### Windows
There has been no attempt to implement or validate cross-platform
functionality in the client.  The system has only been tested on
Linux, and probably has features that would break under Windows,
including:

* Translation of username into userid
* Specification of real/effective uids
* Default "/tmp/pushy" file_dir
* Creation of temporary filenames
* Tests that assume "daemon" is a valid user on the system

The code should be reviewed by a Ruby/Windows expert.

### Inside the server
The server changes are mostly straightforward, but the following design
decisions may be noteworthy:

#### Env representation
The env appears in three formats.  In contexts in which it is part of
a JSON-encoded object (at job creation, in GET results, and in the
0mq "commit" message), it has no encoding outside of its containing
JSON object.  When it is in stored in the DB, it is JSON-encoded separately.
In the pushy-server Erlang record, it is stored as a list of {Key,Value}
pairs.

These choices fit the semantics of the resepective contexts.  In particular
it means that the env is never double-encoded (i.e. into a JSON string,
which is then stored in a JSON object).  The upside of this design is
that the representation is always as perspicuous as possible.  The
downside is that the env gets decoded and encoded multiple times as it
passes through the system.  Specifically, it might follow the following
path:
* Decoded as part of the API job-create request
* Encoded when stored in the database
* Encoded when sent in the commit message
* Encoded when published in the job-start event
* Decoded when extracted from the DB during a job-status query
* Encoded when returned as the result of a job-status query

However, most of these steps are required for all the JSON parameters of a
job.  The only reason the env stands out is that it is separately encoded
and decoded at the database layer.  This is because, from the DB
standpoint, the env is a single unit.  There is no obvious value in
normalizing the data into, e.g., a separate job_env table with one row
per key-value entry.  So _some_ encoding needed to be chosen about how
to represent the [{string,string}] list as a single DB column.  JSON-encoding
seemed like a natural choice.  But it does introduce a bit of confusion.

(BTW, the other solution would be to represent the entire job as a single,
JSON-encoded string in the DB.  This would mean the env no longer was treated
specially; but it would also be a poor data model for postgres database,
preventing efficient queries from being run on anything other than the job-id.)

#### Options in a separate record
The job options are not stored as attributes of the #pushy_job{} record.
Instead, there is a separate record for them.  There is a specific reason
for this: the Chef codebase generally uses a generic mechanism for turning
Erlang records into persistent objects.  It uses some automatic translation
code to convert the record into a row in the database.  (This also explains why
the #pushy_job{} structure is defined in pushy_sql.hrl.)

One limitation of this code is that it requires that the entries in the record
all be defined -- it does an explicit check to throw an error if any fields
are 'undefined'.  This corresponds with all the columns being non-NULLable.

However, the point of these options are that they are **optional**.  Therefore,
the generic mechanism was insufficient.  There were two choices to making
these fields persistent: either to not follow the model of the rest of
the Chef code-base, or to push the options out of the #pushy_job{} record.
The latter choice seemed the smarter one.

