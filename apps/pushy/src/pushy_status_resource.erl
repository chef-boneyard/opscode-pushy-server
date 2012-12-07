%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource for monitoring status of pushy. Extracted from work done by Seth Falcon and Kevin Smith for erchef
%%% @end
-module(pushy_status_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("pushy_wm.hrl").

init(Config) ->
    pushy_wm_base:init(Config).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, #config_state{incarnation_id = IncarnationId} = State) ->
    Json = {[{<<"status">>, <<"it's alive">>},
             {<<"incarnation_id">>, IncarnationId},
             {<<"job_processes">>, get_job_ids()}]},

    {jiffy:encode(Json), Req, State}.

%% Private

get_job_ids() ->
    [JobId || {JobId, _} <- pushy_job_state_sup:get_job_processes()].
