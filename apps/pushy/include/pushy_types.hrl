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
