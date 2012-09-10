%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2011-2012 Opscode Inc.


-record(pushy_state, {
          ctx :: any(),
          incarnation_id :: binary()
         }).

%% TODO: Put these types in "opscode-pushy-server/include/pushy_types.hrl"
-type node_name() :: binary().
-type org_id() :: binary().
-type node_ref() :: {org_id(), node_name()}.
-type job_event() :: ack_commit | nack_commit | ack_run | nack_run | completed | aborted | down.

% FIX: Added some compile-time trickery to make generating
%      node job events easier
-define(DEF_JOB_NODE_EVENT(EventName), EventName(JobId, NodeRef) ->
                                            send_node_event(JobId, NodeRef, EventName)).
