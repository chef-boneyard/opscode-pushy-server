%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author John Keiser <jkeiser@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource for getting information about push jobs
%%% @end
-module(pushy_named_job_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         resource_exists/2,
         to_json/2]).

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

init(Config) ->
    pushy_wm_base:init(Config).
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

malformed_request(Req, State) ->
    pushy_wm_base:malformed_request(Req, State).

is_authorized(Req, State) ->
    pushy_wm_base:is_authorized(Req, State).

forbidden(Req, State) ->
    pushy_wm_base:read_forbidden(Req, State).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
    JobId = iolist_to_binary(wrq:path_info(job_id, Req)),
    case pushy_sql:fetch_job(JobId) of
        {ok, not_found} -> {false, Req, State};
        {ok, Job} -> {true, Req, State#config_state{pushy_job = Job}}
    end.

to_json(Req, #config_state{pushy_job = Job} = State) ->
    {jiffy:encode(job_to_json(Job)), Req, State}.

%{
%  id: 2001,
%  command: "chef-client",
%  status: "complete",
%  run_timeout: 1304914, # Seconds.  Could be null
%  nodes: {
%    "complete": [ "DERPY", "RAINBOWDASH" ]
%  }
%  created_at = "<some date format>",
%  updated_at = "<some date format>"
%}

job_to_json(#pushy_job{
    id = Id,
    command = Command,
    status = Status,
    run_timeout = RunTimeout,
%    created_at = CreatedAt,
%    updated_at = UpdatedAt,
    job_nodes = Nodes
    }) ->
%    CreatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(CreatedAt)),
%    UpdatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(UpdatedAt)),
    NodesJson = job_nodes_json_by_status(Nodes),
    {[ {<<"id">>, iolist_to_binary(Id)},
       {<<"command">>, iolist_to_binary(Command)},
       {<<"status">>, Status},
       {<<"run_timeout">>, RunTimeout},
       {<<"nodes">>, NodesJson}
%       {<<"created_at">>, CreatedAtDate},
%       {<<"updated_at">>, UpdatedAtDate}
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
