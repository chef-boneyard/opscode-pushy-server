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

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            load_from_db(),
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

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_node_state, {pushy_node_state, start_link, []},
            transient, brutal_kill, worker, [pushy_node_state]}]}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

load_from_db() ->
    case pushy_sql:fetch_node_statuses(?POC_ORG_ID) of
        {ok, none} ->
            error_logger:info_msg("No existing node status records found in database, FSM proceses will not be pre-created.");
        {ok, NodeStatuses} ->
            create_processes(NodeStatuses);
        {error, Reason} ->
            error_logger:info_msg("Error loading existing node status records from the database: ~p~n", [Reason])
    end.

create_processes([]) ->
    {ok, done};
create_processes([#pushy_node_status{node_name=NodeName} | Rest]) ->
    new(NodeName),
    create_processes(Rest).
