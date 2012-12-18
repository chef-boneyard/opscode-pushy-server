%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start/1,
         get_process/1,
         get_job_processes/0,
         mark_incomplete_job_nodes_as_crashed/0,
         register_process/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include_lib("pushy_sql.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(Job) ->
    supervisor:start_child(?SERVER, [Job]).

-spec get_process(object_id()) -> pid() | not_found.
get_process(JobId) ->
    try
        gproc:lookup_pid({n,l,{pushy_job, JobId}})
    catch
        error:badarg -> not_found
    end.

-spec get_job_processes() -> [{binary(), pid()}].
get_job_processes() ->
    MatchHead = pushy_util:gproc_match_head(n, l, {pushy_job, '_'}),
    Guard = [],
    Result = ['$$'],
    [{JobId, Pid} || [{_,_,{_,JobId}},Pid,_] <- gproc:select([{MatchHead, Guard, Result}])].

-spec register_process(object_id()) -> boolean().
register_process(JobId) ->
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, {pushy_job, JobId}}),
        pushy_job_monitor:monitor_job(JobId),
        true
    catch
        error:badarg ->
            % This happens when registration fails.  Shut this puppy down, the
            % caller will take care of it!
            lager:error("Failed to register job ~p for PID ~p (already exists as ~p?)",
                        [JobId,self(), gproc:lookup_pid({n,l,JobId}) ]),
            false
    end.

%% Find running job nodes associated with crashed jobs. Mark them as crashed in the db.
mark_incomplete_job_nodes_as_crashed() ->
    case pushy_sql:fetch_incomplete_job_nodes() of
        {ok, Nodes} ->
            [pushy_sql:update_job_node(Node#pushy_job_node{status=crashed})
                || Node <- Nodes];
        {error, Error} -> {error, Error}
    end.

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

mark_incomplete_jobs_as_crashed() ->
    case pushy_sql:fetch_incomplete_jobs() of
        {ok, Jobs} ->
            Update = fun(J) ->
                        pushy_object:update_object(update_job,
                                            J#pushy_job{status=crashed},
                                            J#pushy_job.id)
                     end,
            [ Update(Job) || Job <- Jobs];
        {error, Error} -> {error, Error}
    end.


%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    mark_incomplete_jobs_as_crashed(),
    mark_incomplete_job_nodes_as_crashed(),
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_job_state, {pushy_job_state, start_link, []},
            temporary, brutal_kill, worker, [pushy_job_state]}]}}.
