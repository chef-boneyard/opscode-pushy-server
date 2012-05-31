%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state_sup).

-behaviour(supervisor).

-include_lib("pushy_sql.hrl").

%% API
-export([start_link/0,
         new/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            load_children(),
            {ok, Pid};
        Error ->
            Error
    end.

new(Name, HeartbeatInterval, DeadIntervalCount) ->
    supervisor:start_child(?SERVER, [Name, HeartbeatInterval, DeadIntervalCount]).

new(Name) ->
    error_logger:info_msg("Creating Process For ~s~n", [Name]),
    {ok, HeartbeatInterval} = application:get_env(pushy, heartbeat_interval),
    {ok, DeadIntervalCount} = application:get_env(pushy, dead_interval),
    new(Name, HeartbeatInterval, DeadIntervalCount).

load_children() ->
    load_children(pushy_sql:get_node_statuses(?POC_ORG_ID)).

load_children({ok, []}) ->
    {ok, done};
load_children({ok, [FirstChild | OtherChildren]}) ->
    Name = proplists:get_value(<<"node_name">>, FirstChild),
    new(Name),
    pushy_node_state:down(Name),
    load_children({ok, OtherChildren}).


init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_node_state, {pushy_node_state, start_link, []},
            transient, brutal_kill, worker, [pushy_node_state]}]}}.
