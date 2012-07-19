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
         is_authorized/2,
         resource_exists/2,
         to_json/2]).

-include("pushy_sql.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {
          organization_guid :: string(),
          job :: #pushy_job{} | undefined }).

init(_Config) ->
    % ?debugVal(_Config),
    State = #config_state{},
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

is_authorized(Req, State) ->
    OrgName =  wrq:path_info(organization_id, Req),
    %?debugVal(OrgName),
    State2 = State#config_state{organization_guid = pushy_object:fetch_org_id(OrgName) },
    {true, Req, State2}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
    JobId = wrq:path_info(job_id, Req),
    case pushy_sql:fetch_job(JobId) of
        {ok, not_found} -> false;
        {ok, Job} ->
            {true, Req, State#config_state{job = Job}};
        {error, Error} -> error_response(Req, State, 500, Error)
    end.

to_json(Req, #config_state{job = Job} = State) ->
    {ejson:encode(job_to_json(Job)), Req, State}.

%{
%  id: 2001,
%  command: "chef-client",
%  status: "complete",
%  duration: 1304914, # Seconds.  Could be null
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
    duration = Duration,
    created_at = CreatedAt,
    updated_at = UpdatedAt,
    job_nodes = Nodes
    }) ->
    CreatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(CreatedAt)),
    UpdatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(UpdatedAt)),
    NodesJson = job_nodes_json_by_status(Nodes),
    {[ {<<"id">>, iolist_to_binary(Id)},
       {<<"command">>, iolist_to_binary(Command)},
       {<<"status">>, atom_to_binary(Status, utf8)},
       {<<"duration">>, Duration},
       {<<"nodes">>, NodesJson},
       {<<"created_at">>, CreatedAtDate},
       {<<"updated_at">>, UpdatedAtDate}
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

error_response(Req, State, ErrorCode, Error) ->
    Req2 = wrq:set_resp_body(ejson:encode({[{<<"error">>, Error}]}), Req),
    {{halt, ErrorCode}, Req2, State}.
