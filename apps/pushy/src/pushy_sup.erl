%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2011-2012 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("pushy.hrl").

%% Helper macro for declaring children of supervisor
-define(SUP(I, Args), {I, {I, start_link, Args}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(MWORKER(I, Mod, Args), {I, {Mod, start_link, Args}, permanent, 5000, worker, [I]}).
-define(WORKERNL(I, Args), {I, {I, start, Args}, permanent, 5000, worker, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link(Ctx) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Ctx]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([#pushy_state{ctx=_Ctx} = PushyState]) ->

    %% This attaches the ownership of the ETS table for keys to this process.
    pushy_key_manager:init(),

    %% This attaches the ownership of the ETS table for keys to this process.
    pushy_cache:init([{name, org_guid},
                      {max_size, 10000},
                      {ttl, 100000000}]),

    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                                    [code:priv_dir(pushy), "dispatch.conf"])),
    %% Tell webmachine to pass [IncarnationId] to all resources on init()
    Dispatch2 = [ add_init_params(DispatchLine, PushyState) || DispatchLine <- Dispatch ],

    Port = envy:get(pushy, api_port, integer),
    LogDir = envy:get(pushy, log_dir, string),
    EnableGraphite = envy:get(pushy_common, enable_graphite, boolean),
    WebMachineConfig = [
                        {ip, Ip},
                        {port, Port},
                        {log_dir, LogDir},
                        {dispatch, Dispatch2}],
    MaybeZap = case envy:get(pushy, disable_curve_encryption, false, boolean) of
                   true -> [];
                   false -> [?WORKER(pushy_zap, [PushyState])]
               end,
    Workers1 = [?WORKER(pushy_node_stats_scanner, []),
                ?WORKER(chef_keyring, []),
                ?WORKER(pushy_heartbeat_generator, [PushyState]),
                ?WORKER(pushy_broker, [PushyState])],
    Switches = start_switches(1, PushyState, []),
    Workers2 = [?WORKER(pushy_process_monitor, [pushy_command_switch, pushy_command_switch:switch_processes_fun(), 1000]),
                ?SUP(pushy_node_state_sup, []),
                ?SUP(pushy_job_state_sup, []),
                ?SUP(pushy_org_events_sup, []),
                ?WORKER(pushy_job_monitor, []),
                ?WORKERNL(webmachine_mochiweb, [WebMachineConfig])  %% FIXME start or start_link here?
               ],
    Workers = MaybeZap ++ Workers1 ++ Switches ++ Workers2,
    pushy_node_stats:init(),
    {ok, {{one_for_one, 60, 120},
         maybe_run_graphite(EnableGraphite, Workers)}}.

maybe_run_graphite(true, Workers) ->
    [?SUP(folsom_graphite_sup, []) | Workers];
maybe_run_graphite(false, Workers) ->
    Workers.

add_init_params({["organizations"|_Tail]=Route,
                 pushy_config_resource=Resource, []},
                #pushy_state{incarnation_id = IncarnationId,
                             curve_public_key = CurvePublicKey}) ->
    {Route, Resource, [{incarnation_id, IncarnationId}, {curve_public_key, CurvePublicKey}]};
add_init_params({["organizations"|_Tail]=Route, Resource, []},
                #pushy_state{incarnation_id = IncarnationId}) ->
    {Route, Resource, [{incarnation_id, IncarnationId}]};
add_init_params({["pushy"|_Tail]=Route, Resource, []},
                #pushy_state{incarnation_id = IncarnationId}) ->
    {Route, Resource, [{incarnation_id, IncarnationId}]};
add_init_params(Other, _PushyState) ->
    Other.

start_switches(0, _PushyState, Switches) ->
    Switches;
start_switches(Count, PushyState, Switches) ->
    Id = list_to_atom("pushy_command_switch_" ++ integer_to_list(Count)),
    start_switches(Count - 1, PushyState,
                   [?MWORKER(Id, pushy_command_switch, [PushyState, Count]) | Switches]).
