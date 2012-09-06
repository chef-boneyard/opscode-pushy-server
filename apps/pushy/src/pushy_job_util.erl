%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Utility functions for common job tasks
%%% @end
-module(pushy_job_util).

-include("pushy_sql.hrl").

-export([
        jobs_to_json/1,
        job_to_json/1
    ]).



jobs_to_json(Jobs) ->
    [job_to_json(Job) || Job <- Jobs].


%%  {
%%    id: 2001,
%%    command: "chef-client",
%%    status: "complete",
%%    duration: 1304914, # Seconds.  Could be null
%%    nodes: {
%%      "complete": [ "DERPY", "RAINBOWDASH" ]
%%    }
%%    created_at = "<some date format>",
%%    updated_at = "<some date format>"
%%  }

job_to_json(#pushy_job{
    id = Id,
    command = Command,
    status = Status,
    finished_reason = Reason,
    job_nodes = Nodes
    }) ->
    NodesJson = job_nodes_json_by_status(Nodes),
    {[ {<<"id">>, iolist_to_binary(Id)},
       {<<"command">>, iolist_to_binary(Command)},
       {<<"status">>, atom_to_binary(case Status of finished -> Reason; _ -> Status end, utf8)},
       {<<"duration">>, 300},
       {<<"nodes">>, NodesJson}
    ]}.

job_nodes_json_by_status(Nodes) ->
    NodesByStatus = job_nodes_by_status(Nodes, dict:new()),
    {[
        { erlang:atom_to_binary(Status, utf8), dict:fetch(Status, NodesByStatus) }
        || Status <- dict:fetch_keys(NodesByStatus)
    ]}.

job_nodes_by_status([], Dict) ->
    Dict;
job_nodes_by_status([#pushy_job_node{node_name = Name, status = Status} | Nodes], Dict) ->
    Dict2 = dict:append(Status, Name, Dict),
    job_nodes_by_status(Nodes, Dict2).
