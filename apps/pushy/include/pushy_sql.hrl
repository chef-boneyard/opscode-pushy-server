-type id() :: binary().
%% object ids are always 32 characters hex. This spec matches the
%% length, might be able to constrain further for range of elements.
-type object_id() :: <<_:256>>.

%% job status
-type job_status() :: new |
                      voting |
                      running |
                      complete |
                      quorum_failed |
                      crashed |
                      aborted |
                      timed_out.

-type job_node_status() :: new |
                           ready |
                           running |
                           succeeded |
                           failed |
                           aborted |
                           unavailable |
                           nacked |
                           crashed |
                           was_ready |
                           timed_out.

-record(pushy_job_node, {'job_id'::object_id(),              % guid for object (unique)
                         'org_id'::object_id(),              % organization guid
                         'node_name'::binary(),              % node name
                         'status'::job_node_status(),        % node's status in context of job
                         'created_at'::binary(),  % time created at
                         'updated_at'::binary()   % time updated at
                         }).

-record(pushy_job, {'id'::object_id(),                  % guid for object (unique)
                    'org_id'::object_id(),              % organization guid
                    'command'::binary(),                % command to execute
                    'quorum'::non_neg_integer(),        % quorum count
                    'status'::job_status(),             % job status
                    'run_timeout'::non_neg_integer(),   % max duration (in seconds) to allow execution
                    'job_nodes' = [] ::[#pushy_job_node{}],
                    'last_updated_by'::object_id(),     % authz guid of last actor to update
                    'created_at'::binary(),  % time created at
                    'updated_at'::binary()  % time updated at
                    }).

-type pushy_object() :: #pushy_job{}.
