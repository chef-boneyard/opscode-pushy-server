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
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         to_json/2]).

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

init(_Config) ->
    % ?debugVal(_Config),
    State = #config_state{},
    {ok, State}.
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

to_json(Req, #config_state{organization_guid=OrgId}=State) ->
    {ok, StatusList} = pushy_sql:fetch_node_statuses(OrgId),

    ConfigurationStruct = [node_to_json_struct(E) || E <- StatusList],
    %% ?debugVal(ConfigurationStruct),
    ConfigurationJson = jiffy:encode(ConfigurationStruct),
    {ConfigurationJson, Req, State}.

node_to_json_struct(#pushy_node_status{node_name=Name, status=Status, updated_at=UpdatedAt}) ->
    UpdatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(UpdatedAt)),
    {[ {<<"node_name">>, Name},
       {<<"status">>, Status},
       {<<"updated_at">>, UpdatedAtDate}
     ]}.
