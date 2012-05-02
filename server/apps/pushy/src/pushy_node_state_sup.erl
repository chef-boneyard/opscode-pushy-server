%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new(Name, HeartbeatInterval, DeadIntervalCount) ->
    supervisor:start_child(?SERVER, [Name, HeartbeatInterval, DeadIntervalCount]).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_node_state, {pushy_node_state, start_link, []},
            transient, brutal_kill, worker, [pushy_node_state]}]}}.
