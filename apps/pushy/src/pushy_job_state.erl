%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         node_execution_state_updated/3,
         get_job_state/1]).

%% gen_fsm callbacks
-export([init/1,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pushy.hrl").
-include_lib("pushy_sql.hrl").

-record(state,
        {
            job            :: #pushy_job{},
            job_nodes      :: dict(), % dict(node_ref())
            up_down_status :: dict() % dict(node_ref(), node_status())
        }).

%%%
%%% External API
%%%
-spec start_link(#pushy_job{}) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Job) ->
    gen_fsm:start_link(?MODULE, Job, []).

-spec node_execution_state_updated(object_id(), node_ref(), job_node_status()) -> ok.
node_execution_state_updated(JobId, NodeRef, NewState) ->
    lager:info("---------> job:node_execution_state_updated(~p, ~p -> ~p)", [JobId, NodeRef, NewState]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_all_state_event(Pid, {node_execution_state_updated,NodeRef,NewState}).

-spec get_job_state(object_id()) -> #pushy_job{} | not_found.
get_job_state(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> gen_fsm:sync_send_all_state_event(Pid, get_job_status)
    end.


%
% Init is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
-spec init(#pushy_job{}) ->
    {'ok', job_status(), #state{}} |
    {'stop', 'shutdown', #state{}}.
init(#pushy_job{id = JobId, job_nodes = JobNodeList} = Job) ->
    case pushy_job_state_sup:register_process(JobId) of
        true ->
            JobNodes = dict:from_list([
                {{OrgId, NodeName}, JobNode} ||
                #pushy_job_node{org_id = OrgId, node_name = NodeName} = JobNode <- JobNodeList
            ]),
            UpDown = initialize_up_down(dict:fetch_keys(JobNodes)),
            State = #state{
                job = Job#pushy_job{job_nodes = undefined},
                job_nodes = JobNodes,
                up_down_status = UpDown
            },
            {next_state, ResultState, State2} = set_state(voting, undefined, State),
            {ok, ResultState, State2};
        false ->
            {stop, shutdown, undefined}
    end.

initialize_up_down(JobNodes) ->
    dict:from_list([ {NodeRef, initialize_node_up_down(NodeRef)} || NodeRef <- JobNodes ]).

initialize_node_up_down(NodeRef) ->
    pushy_node_state:start_watching(NodeRef),
    pushy_node_state:current_state(NodeRef).

%
% Events
%

-spec handle_event(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_event({node_execution_state_updated,NodeRef,NodeState}, StateName, State) ->
    update_node_execution_state(NodeRef,NodeState,StateName,State);
handle_event(Event, StateName, State) ->
    lager:error("Unknown message handle_event(~p)", [Event]),
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), job_status(), #state{}) ->
        {'reply', 'ok', job_status(), #state{}}.
handle_sync_event(get_job_status, _From, StateName,
        #state{job = Job, job_nodes = JobNodes} = State) ->
    JobNodesList = [ JobNode || {_,JobNode} <- dict:to_list(JobNodes) ],
    Job2 = Job#pushy_job{job_nodes = JobNodesList},
    {reply, Job2, StateName, State};
handle_sync_event(Event, From, StateName, State) ->
    lager:error("Unknown message handle_sync_event(~p) from ~p", [Event, From]),
    {reply, ok, StateName, State}.

-spec handle_info(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_info({node_state_change, NodeRef, NodeUpDown},
            StateName, #state{up_down_status = UpDown} = State) ->
    % TODO only do this if it changes, not all the time
    lager:info("--------> node_state_change ~p -> ~p", [NodeRef, NodeUpDown]),
    State2 = State#state{up_down_status = dict:store(NodeRef, NodeUpDown, UpDown)},
    kick_node_towards_desired_state(StateName, State2, NodeRef),
    detect_aggregate_state_change(StateName, State2);
handle_info(Info, StateName, State) ->
    lager:error("Unknown message handle_info(~p)", [Info]),
    {next_state, StateName, State}.

-spec terminate(any(), job_status(), #state{}) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(any(), job_status(), #state{}, any()) ->
        {'ok', job_status(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%
% PRIVATE
%
% This is called to check whether we are ready to proceed to a different state
% or not.
-spec detect_aggregate_state_change(job_status(), #state{}) ->
    {'next_state', job_status(), #state{}}.
detect_aggregate_state_change(voting, #state{job_nodes = JobNodes, up_down_status = UpDown}=State) ->
    {NewAndUp, WontRun} = dict:fold(
        fun(NodeRef, #pushy_job_node{status = NodeState}, {NewAndUp, WontRun}) ->
            case NodeState of
                never_ran -> {NewAndUp, WontRun+1};
                new -> case dict:fetch(NodeRef, UpDown) of
                           up -> {NewAndUp+1, WontRun};
                           down -> {NewAndUp, WontRun+1}
                       end;
                _ -> {NewAndUp, WontRun}
            end
        end, {0, 0}, JobNodes),
    if
        % If any nodes are new and still up, we're not yet ready; stay voting.
        NewAndUp > 0 -> {next_state, voting, State};
        % If any nodes are finished or new and down (i.e. won't ever be ready), quorum fails.
        WontRun > 0 -> set_state(finished, quorum_failed, State);
        % Otherwise, all remaining nodes are new+down, ready, or running, and we can proceed.
        true -> set_state(running, undefined, State)
    end;
detect_aggregate_state_change(running, #state{job_nodes = JobNodes, up_down_status = UpDown}=State) ->
    % If any node is unfinished and still up, we are unfinished.
    UnfinishedAndUp = dict:fold(
        fun(NodeRef, #pushy_job_node{status = NodeState}, UnfinishedAndUp) ->
            case NodeState of
                never_ran -> UnfinishedAndUp;
                complete -> UnfinishedAndUp;
                aborted -> UnfinishedAndUp;
                _ -> case dict:fetch(NodeRef, UpDown) of
                         up -> UnfinishedAndUp+1;
                         down -> UnfinishedAndUp
                     end
            end
        end, 0, JobNodes),
    if
        UnfinishedAndUp > 0 -> {next_state, running, State};
        true -> set_state(finished, complete, State)
    end;
detect_aggregate_state_change(finished, State) ->
    % Homey don't change state.
    {next_state, finished, State}.

-spec update_node_execution_state(node_ref(), job_node_status(), job_status(), #state{}) ->
    {'next_state', job_status(), #state{}}.
update_node_execution_state(NodeRef,NodeState,StateName,
    #state{job_nodes = JobNodes} = State) ->
    lager:info("JOB NODE ~p -> ~p", [NodeRef, NodeState]),
    % TODO handle incorrect org id on node.
    % TODO handle missing node.
    % TODO handle invalid transitions.
    JobNodes2 = dict:update(NodeRef,
        fun(JobNode) -> JobNode#pushy_job_node{status = NodeState} end,
        JobNodes),
    State2 = State#state{job_nodes = JobNodes2},
    % TODO save job node.
    kick_node_towards_desired_state(StateName, State2, NodeRef),
    detect_aggregate_state_change(StateName, State2).

% Called on transition to a new state
-spec set_state(job_status(), job_finished_reason(), #state{}) ->
    {'next_state', job_status(), #state{}}.
set_state(StateName, FinishedReason, #state{job = Job} = State) ->
    lager:info("JOB -> ~p (~p)", [StateName, FinishedReason]),
    Job2 = Job#pushy_job{status = StateName, finished_reason = FinishedReason},
    State2 = State#state{job = Job2},
    kick_nodes_towards_desired_state(StateName, State2),
    detect_aggregate_state_change(StateName, State2).

-spec kick_nodes_towards_desired_state(job_status(), #state{}) -> ok.
kick_nodes_towards_desired_state(StateName, #state{job_nodes = JobNodes}=State) ->
    dict:fold(
        fun(NodeRef, #pushy_job_node{status = NodeState}, _) ->
            kick_node_towards_desired_state(StateName, State, NodeRef, NodeState)
        end, nop, JobNodes).

-spec kick_node_towards_desired_state(job_status(), #state{}, node_ref()) -> ok.
kick_node_towards_desired_state(StateName, #state{job_nodes = JobNodes} = State, NodeRef) ->
    JobNode = dict:fetch(NodeRef, JobNodes),
    kick_node_towards_desired_state(StateName, State, NodeRef, JobNode#pushy_job_node.status).

-spec kick_node_towards_desired_state(job_status(), #state{}, node_ref(), job_node_status()) -> ok.
kick_node_towards_desired_state(voting, State, NodeRef, new) ->
    send_message(<<"job_command">>, State, NodeRef);
kick_node_towards_desired_state(running, State, NodeRef, new) ->
    send_message(<<"job_command">>, State, NodeRef);
kick_node_towards_desired_state(running, State, NodeRef, ready) ->
    send_message(<<"job_execute">>, State, NodeRef);
% TODO If job is finished, immediately move new to never_ran
kick_node_towards_desired_state(finished, State, NodeRef, ready) ->
    send_message(<<"job_release">>, State, NodeRef);
kick_node_towards_desired_state(finished, State, NodeRef, running) ->
    send_message(<<"job_abort">>, State, NodeRef);
kick_node_towards_desired_state(_, _, _, _) ->
    % Nobody else gets kicked; they are at or beyond where they need to be.
    ok.

-spec send_message(binary(), #state{}, node_ref()) -> 'ok'.
send_message(Type, #state{job = Job}, NodeRef) ->
    lager:info("Kicking node ~p with ~p", [NodeRef, Type]),
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    Message = [
        {type, Type},
        {job_id, Job#pushy_job.id},
        {server, list_to_binary(Host)},
        {command, Job#pushy_job.command}
    ],
    pushy_command_switch:send_command(NodeRef, jiffy:encode({Message})).
