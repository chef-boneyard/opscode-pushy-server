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
    {ok, {{one_for_one, 3, 60},
          [?SUP(pushy_node_state_sup, []),
           ?WORKER(chef_keyring, []),
           ?WORKER(pushy_heartbeat_generator, [Ctx]),
           ?WORKER(pushy_node_status_tracker, [Ctx])]}}.
