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
    application:set_env(pushy, heartbeat_interval, 1000),
    application:set_env(pushy, server_heartbeat_socket, "tcp://*:5559"),
    {ok, {{one_for_one, 3, 60},
          [?SUP(pushy_node_state_sup, []),
           ?WORKER(pushy_heartbeat_generator, [Ctx])]}}.
