%%%-------------------------------------------------------------------
%%% @author Matthew Peck
%%% @copyright 2012 Opscode, Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pushy_job_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         monitor_job/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("pushy_sql.hrl").


-record(state, {jobs :: dict()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #state{}, []).

monitor_job(JobId) ->
    gen_server:cast(?MODULE, {monitor, JobId}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(State) ->
    lager:info("Starting job monitor"),
    {ok, State#state{jobs = dict:new()}}.

handle_call(Msg, _From, State) ->
    lager:warn("Unknown message handle_call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({monitor, JobId}, #state{jobs = Jobs} = State) ->
    Pid = pushy_job_state_sup:get_process(JobId),
    erlang:monitor(process, Pid),
    State1 = State#state{jobs = dict:append(Pid, JobId, Jobs)},
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:warn("Unknown message handle_cast: ~p~n", [Msg]),
    {noreply, State}.

%% If we shutdown normally do nothing
handle_info({'DOWN', _Ref, process, Pid, {shutdown, _Reason}},
        #state{jobs = Jobs} = State) ->
    {noreply, State#state{jobs=dict:erase(Pid, Jobs)}};
%% Otherwise we have crashed so we need to update the job record
handle_info({'DOWN', _Ref, process, Pid, _Reason},
        #state{jobs = Jobs} = State) ->
    case dict:find(Pid, Jobs) of
        {ok, JobId} ->
            {ok, Job} = pushy_sql:fetch_job(JobId),
            pushy_object:update_object(update_job,
                Job#pushy_job{status=crashed}, JobId);
        error ->
            lager:error("JobId not found"),
            error
    end,
    {noreply, State#state{jobs=dict:erase(Pid, Jobs)}};
handle_info(Msg, State) ->
    lager:warn("Unknown message handle_info: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
