%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state_sup).

-behaviour(supervisor).

-include_lib("pushy.hrl").
-include_lib("pushy_sql.hrl").

%% API
-export([start_link/0,
         get_or_create_process/1,
         get_process/1,
         mk_gproc_name/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec get_or_create_process(node_ref()) -> pid().
get_or_create_process(NodeRef) ->
    GprocName = mk_gproc_name(NodeRef),
    case catch gproc:lookup_pid({n,l,GprocName}) of
        {'EXIT', _} ->
            % Run start_child asynchronously; we only need to wait until the
            % process registers itself before we can send it messages.
            spawn(supervisor, start_child, [?SERVER, [NodeRef]]),
            {Pid, _Value} = gproc:await({n,l,GprocName},1000),
            Pid;
        Pid -> Pid
    end.

get_process(NodeRef) ->
    GprocName = mk_gproc_name(NodeRef),
    case catch gproc:lookup_pid({n,l,GprocName}) of
        {'EXIT', _} ->
            undefined;
        Pid -> Pid
    end.

-spec mk_gproc_name(node_ref()) -> {'heartbeat', org_id(), node_name()}.
mk_gproc_name({OrgId, NodeName}) when is_binary(OrgId) andalso is_binary(NodeName) ->
    {heartbeat, OrgId, NodeName}.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 60, 120},
          [{pushy_node_state, {pushy_node_state, start_link, []},
            transient, brutal_kill, worker, [pushy_node_state]}]}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

