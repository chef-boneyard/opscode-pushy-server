%%%-------------------------------------------------------------------
%%% @author Matthew Peck
%%% @copyright 2012 Opscode, Inc.
%%% @doc Monitor a set of pushy_job_state processes and clean up
%%% the state in the database on job_state process crashes
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pushy_job_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         monitor_job/2,
         is_monitored/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("pushy_sql.hrl").

-compile([{parse_transform, lager_transform}]).

-record(state, {jobs :: dict()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #state{}, []).

%% @doc Monitor a Pushy Job FSM, specified by the JobId and its Pid
monitor_job(JobId, Pid) ->
    gen_server:cast(?MODULE, {monitor, JobId, Pid}).

%% @doc Is a given Pid being monitored
is_monitored(Pid) ->
    gen_server:call(?MODULE, {is_monitored, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(State) ->
    lager:info("Starting job monitor"),
    {ok, State#state{jobs = dict:new()}}.

handle_call({is_monitored, Pid}, _From, #state{jobs = Jobs} =State) ->
    {reply, dict:is_key(Pid, Jobs), State};
handle_call(Msg, _From, State) ->
    lager:warn("Unknown message handle_call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({monitor, JobId, Pid}, #state{jobs = Jobs} = State) ->
    erlang:monitor(process, Pid),
    State1 = State#state{jobs = dict:store(Pid, JobId, Jobs)},
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
            ok = mark_as_crashed(Job);
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
%%
%% Internal functions
%%
mark_as_crashed(#pushy_job{id = JobId,
                           job_nodes = JobNodes} = Job) ->
    case pushy_object:update_object(update_job,
                                    Job#pushy_job{status=crashed},
                                    JobId) of
    {ok, 1} ->
        %% Now we send nodes in the job to rehab
        nodes_to_rehab(JobNodes);
    {ok, not_found} ->
        lager:warning("Couldn't find job ~p in DB when cleaning up crashed job", [JobId]),
        %% Not much to do about it, so return ok
        ok;
    {error, Error} ->
        {error, Error}
  end.

-spec nodes_to_rehab(JobNodes :: [#pushy_job_node{}]) -> ok | {error, term()}.
nodes_to_rehab([]) ->
    ok;
nodes_to_rehab([#pushy_job_node{org_id = OrgId,
                                node_name = NodeName} | Rest]) ->
    case pushy_node_state:rehab({OrgId, NodeName}) of
        undefined ->
            lager:info("Tried to send node {~p, ~p}, into rehab but it wasn't there", [OrgId, NodeName]);
        ok ->
            ok
    end,
    nodes_to_rehab(Rest).
