-type node_name() :: binary().
-type org_id() :: binary().
-type node_ref() :: {org_id(), node_name()}.
-type node_addr() :: binary().
-type job_event() :: ack_commit | nack_commit | ack_run | nack_run | succeeded | failed | aborted.
-type node_event() :: heartbeat | job_event().

% node status
-type node_status() :: online |
                       offline.

% node availablity
-type node_availability() :: available |
                             unavailable.

-type incarnation_id() :: binary().

%% TODO: this seems as though it could be tightened up
%%
%% TODO: The binary expression is the same as object_id() in
%% pushy_sql.hrl.  We need to do a bit of type refactoring so that we
%% can directly use that type here, instead of duplicating.
-type job_id() :: <<_:256>> | null | invalid_job_id.
