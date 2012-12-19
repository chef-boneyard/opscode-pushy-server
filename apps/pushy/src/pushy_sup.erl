%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

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
                        {dispatch, Dispatch2},
                        {enable_perf_logger, true}],
    Workers1 = [?WORKER(pushy_node_stats_scanner, []),
                ?WORKER(chef_keyring, []),
                ?WORKER(pushy_heartbeat_generator, [PushyState]),
                ?WORKER(pushy_broker, [PushyState])],
    Switches = start_switches(5, PushyState, []),
    Workers2 = [?SUP(pushy_node_state_sup, []),
                ?SUP(pushy_job_state_sup, []),
                ?WORKER(pushy_job_monitor, []),
                ?WORKER(pushy_node_status_updater, []),
                ?WORKERNL(webmachine_mochiweb, [WebMachineConfig])  %% FIXME start or start_link here?
               ],
    Workers = Workers1 ++ Switches ++ Workers2,
    pushy_node_stats:init(),
    {ok, {{one_for_one, 60, 120},
         maybe_run_graphite(EnableGraphite, Workers)}}.

maybe_run_graphite(true, Workers) ->
    [?SUP(folsom_graphite_sup, []) | Workers];
maybe_run_graphite(false, Workers) ->
    Workers.

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
