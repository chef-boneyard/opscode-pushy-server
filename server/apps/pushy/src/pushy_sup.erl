%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:set_env(pushy, heartbeat_interval, 1000),
    application:set_env(pushy, heartbeat_source_socket, "tcp://*:5556"),
    {ok, {{one_for_one, 3, 60},
          [{pushy_node_state_sup, {pushy_node_state_sup, start_link, []},
            permanent, infinity, supervisor, [pushy_node_state_sup]},
           {pushy_heartbeat_generator, {pushy_heartbeat_generator, start_link, []},
            permanent, infinity, worker, [pushy_heartbeat_generator]}
          ]}}.

