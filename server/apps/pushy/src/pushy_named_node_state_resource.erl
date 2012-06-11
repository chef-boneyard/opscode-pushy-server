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
    ?debugVal(_Config),
    State = #config_state{},
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

is_authorized(Req, State) ->
    OrgId =  wrq:path_info(organization_id, Req),
    %?debugVal(OrgName),
    % OrgGuid = OrgName, %%% TODO: Need to actually look this up! see chef_common chef_db:fetch_org_id for an approach.
    State2 = State#config_state{orgname = OrgId},
    {true, Req, State2}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    OrgName = wrq:path_info(organization_id, Req),

    NodeName = wrq:path_info(node_name, Req),
    {ok, StatusList} = pushy_sql:fetch_node_status(OrgName, NodeName),

    ConfigurationStruct = [node_to_json_struct(E) || E <- StatusList],
    ConfigurationJson = ejson:encode(ConfigurationStruct),
    {ConfigurationJson, Req, State}.

node_to_json_struct(#pushy_node_status{node_name=Name, status=Status, updated_at=UpdatedAt}) ->
    UpdatedAtDate =  iolist_to_binary(httpd_util:rfc1123_date(UpdatedAt)),
    {[ {<<"node_name">>, Name},
       {<<"status">>, Status},
       {<<"updated_at">>, UpdatedAtDate}
     ]}.
