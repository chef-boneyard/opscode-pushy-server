%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
-module(pushy_node_states_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {
          orgname :: string(),
          organization_guid :: string() }).

init(_Config) ->
    ?debugVal(_Config),
    State = #config_state{},
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

%is_authorized(Req, State) ->
%    OrgName =  wrq:path_info(Req, organization),
%    ?debugVal(OrgName),
%    State2 = State#config_state{orgname = OrgName},
%    {{true, foo}, Req, State2}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->

    OrgId = <<"ORG">>,

    {ok, StatusList} = pushy_sql:fetch_node_statuses(OrgId),
    ?debugVal(StatusList),

    ConfigurationStruct = {[{<<"status">>, StatusList}]},
    ?debugVal(ConfigurationStruct),

    ConfigurationJson = ejson:encode(ConfigurationStruct),

    ?debugVal(ConfigurationJson),

    {ConfigurationJson, Req, State}.

