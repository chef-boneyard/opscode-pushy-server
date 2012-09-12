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
    %% DEBUGGING AID; having the context available can help
    %% But this is a bad idea to do with nif hooks.
    %% Tbl = ets:new(debug, [named_table]),
    %% ets:insert(Tbl, {context, Ctx}),

    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                                    [code:priv_dir(pushy), "dispatch.conf"])),

%%% Set up trace dir specific stuff
%%%    {_,_,[{trace_dir, TraceDir}]} = lists:keyfind(["dev", "wmtrace", '*'], 1, Dispatch),
%%%    ok = filelib:ensure_dir(string:join([TraceDir,"trace"], "/")),

    Port = pushy_util:get_env(pushy, api_port, fun is_integer/1),
    LogDir = pushy_util:get_env(pushy, log_dir, fun is_list/1),
    WebMachineConfig = [
                        {ip, Ip},
                        {port, Port},
                        {log_dir, LogDir},
                        {dispatch, Dispatch},
                        {enable_perf_logger, true}],
    ?debugVal(WebMachineConfig),
    {ok, {{one_for_one, 60, 120},
               [?SUP(pushy_node_state_sup, []),
                ?SUP(pushy_job_state_sup, []),
                ?SUP(folsom_sup, []),
                ?WORKER(chef_keyring, []),
                ?WORKER(pushy_node_status_updater, []),
                ?WORKER(pushy_heartbeat_generator, [PushyState]),
                ?WORKER(pushy_command_switch, [PushyState]),
                ?WORKERNL(webmachine_mochiweb, [WebMachineConfig])  %% FIXME start or start_link here?
               ]}}.

