%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(pushy_job_runner_sup).

-behaviour(supervisor).

-include_lib("pushy_sql.hrl").

%% API
-export([start_link/0,
         execute/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% Creates a gen_fsm tracker for the job
execute(JobId) ->
    lager:info("Creating runner process for job [~s]", [JobId]),
    supervisor:start_child(?SERVER, [JobId]).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{pushy_job_runner, {pushy_job_runner, start_link, []},
            transient, brutal_kill, worker, [pushy_job_runner]}]}}.
