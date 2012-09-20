%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

-include("pushy.hrl").

%% Helper macro for declaring children of supervisor
-define(SUP(I, Args), {I, {I, start_link, Args}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
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

    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                                    [code:priv_dir(pushy), "dispatch.conf"])),

%%% Set up trace dir specific stuff
%%%    {_,_,[{trace_dir, TraceDir}]} = lists:keyfind(["dev", "wmtrace", '*'], 1, Dispatch),
%%%    ok = filelib:ensure_dir(string:join([TraceDir,"trace"], "/")),

    Port = envy:get(pushy, api_port, integer),
    LogDir = envy:get(pushy, log_dir, string),
    EnableGraphite = envy:get(pushy_common, enable_graphite, boolean),
    WebMachineConfig = [
                        {ip, Ip},
                        {port, Port},
                        {log_dir, LogDir},
                        {dispatch, Dispatch},
                        {enable_perf_logger, true}],
    ?debugVal(WebMachineConfig),
    Workers = [?SUP(pushy_node_state_sup, []),
                ?SUP(pushy_job_state_sup, []),
                ?WORKER(chef_keyring, []),
                ?SUP(folsom_sup, []),
                ?WORKER(pushy_node_status_updater, []),
                ?WORKER(pushy_heartbeat_generator, [PushyState]),
                ?WORKER(pushy_command_switch, [PushyState]),
                ?WORKERNL(webmachine_mochiweb, [WebMachineConfig])  %% FIXME start or start_link here?
               ],
    {ok, {{one_for_one, 60, 120},
         maybe_run_graphite(EnableGraphite, Workers)}}.


maybe_run_graphite(true, Workers) ->
    [?SUP(folsom_graphite_sup, []) | Workers];
maybe_run_graphite(false, Workers) ->
    Workers.
