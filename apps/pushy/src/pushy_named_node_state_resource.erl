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
         malformed_request/2,
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

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, #config_state{organization_guid=OrgId}=State) ->
    NodeName = list_to_binary(wrq:path_info(node_name, Req)),
    % TODO handle missing node
    {NodeState, {Availability, _Job}} = pushy_node_state:status({OrgId, NodeName}),
    Result = jiffy:encode({[
        {<<"node_name">>, NodeName},
        {<<"status">>, NodeState},
        {<<"availability">>, Availability}
    ]}),
    {Result, Req, State}.
