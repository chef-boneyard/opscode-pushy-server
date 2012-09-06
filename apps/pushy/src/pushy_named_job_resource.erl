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

-record(state, {
          organization_guid :: string(),
          job :: #pushy_job{}
          }).

init(_Config) ->
    % ?debugVal(_Config),
    State = #state{},
%%    {ok, State}.
    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

is_authorized(Req, State) ->
    OrgName =  wrq:path_info(organization_id, Req),
    %?debugVal(OrgName),
    State2 = State#state{organization_guid = pushy_object:fetch_org_id(OrgName) },
    {true, Req, State2}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
    JobId = iolist_to_binary(wrq:path_info(job_id, Req)),
    case pushy_job_state:get_job_state(JobId) of
        not_found -> {false, Req, State};
        Job -> {true, Req, State#state{job = Job}}
    end.

to_json(Req, #state{job = Job} = State) ->
    {jiffy:encode(pushy_job_util:job_to_json(Job)), Req, State}.

