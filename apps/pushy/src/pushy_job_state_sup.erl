%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start/1,
         get_process/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type job_id() :: binary().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(Job) ->
    supervisor:start_child(?SERVER, [Job]).

-spec get_process(job_id()) -> pid().
get_process(JobId) ->
    gproc:lookup_pid({n,l,JobId}).
    % TODO this is what we'll need to do when jobs can be loaded from the DB.
    %case catch gproc:lookup_pid({n,l,JobId}) of
    %    {'EXIT', _} ->
    %        case supervisor:start_child(?SERVER, [JobId]) of
    %            {error,{already_started,Pid}} -> Pid;
    %            {ok,Pid} -> Pid
    %        end;
    %    Pid -> Pid
    %end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_job_state, {pushy_job_state, start_link, []},
            transient, brutal_kill, worker, [pushy_job_state]}]}}.
