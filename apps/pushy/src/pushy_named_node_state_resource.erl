%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
-module(pushy_named_node_state_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

-include("pushy_sql.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {
          orgname :: string(),
          organization_guid :: string() }).

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

to_json(Req, #config_state{organization_guid=OrgId}=State) ->
    NodeName = list_to_binary(wrq:path_info(node_name, Req)),
    % TODO handle missing node
    NodeState = pushy_node_state:current_state({OrgId, NodeName}),
    InRehab = pushy_node_state:in_rehab({OrgId, NodeName}),
    Result = jiffy:encode({[
        {<<"node_name">>, NodeName},
        {<<"status">>, NodeState},
        {<<"is_in_rehab">>, InRehab}
    ]}),
    {Result, Req, State}.
