%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_execution_state_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         get_process/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_process(OrgId, NodeName) ->
    case catch gproc:lookup_pid({n,l,{node_execution_state,OrgId,NodeName}}) of
        {'EXIT', _} ->
            % If the caller issues a fire-and-forget message, he doesn't need to
            % wait for the child to completely initialize; messages will be
            % queued for when the process is ready.  We'll just wait until we
            % have the process's pid from gproc.
            spawn(supervisor, start_child, [?SERVER, [OrgId, NodeName]]),
            {Pid, _Value} = gproc:await({n,l,{node_execution_state,OrgId,NodeName}},1000),
            Pid;
        Pid -> Pid
    end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_node_execution_state, {pushy_node_execution_state, start_link, []},
            transient, brutal_kill, worker, [pushy_node_execution_state]}]}}.
