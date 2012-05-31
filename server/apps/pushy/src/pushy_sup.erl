%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SUP(I, Args), {I, {I, start_link, Args}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Ctx) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Ctx]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Ctx]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                                    [code:priv_dir(pushy), "dispatch.conf"])),

%%% Set up trace dir specific stuff
%%%    {_,_,[{trace_dir, TraceDir}]} = lists:keyfind(["dev", "wmtrace", '*'], 1, Dispatch),
%%%    ok = filelib:ensure_dir(string:join([TraceDir,"trace"], "/")),

    Port = case application:get_env(pushy, api_port) of
               {ok, P} -> P;
               undefined ->
                   fast_log:todo() %% FIXME
           end,
    LogDir = case application:get_env(pushy, log_dir) of
                 {ok, LD} -> LD;
                 undefined -> "priv/log"
             end,
    WebMachineConfig = [
                        {ip, Ip},
                        {port, Port},
                        {log_dir, LogDir},
                        {dispatch, Dispatch},
                        {enable_perf_logger, true}],
    {ok, {{one_for_one, 3, 60},
          [?SUP(pushy_node_state_sup, []),
           ?WORKER(chef_keyring, []),
           ?WORKER(pushy_heartbeat_generator, [Ctx]),
           ?WORKER(pushy_node_status_tracker, [Ctx]),
           ?WORKER(webmachine_mochiweb, [WebMachineConfig]) ]}}. %% FIXME start or start_link here?
