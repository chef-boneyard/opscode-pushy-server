%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @doc
%%% REST resource for monitoring status of pushy. Extracted from work done by Seth Falcon and Kevin Smith for erchef
%%% @end
%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
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
             {<<"node_fsm_count">>, get_node_fsm_count()},
             {<<"job_processes">>, get_job_ids()}]},

    {jiffy:encode(Json), Req, State}.

%% Private

get_job_ids() ->
    [JobId || {JobId, _} <- pushy_job_state_sup:get_job_processes()].

get_node_fsm_count() ->
    Nodes = pushy_node_state_sup:get_heartbeating_nodes(),
    length(Nodes).
