%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         node_execution_state_updated/3,
         node_execution_finished/3,
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
            job         :: #pushy_job{}
        }).

%%%
%%% External API
%%%
-spec start_link(#pushy_job{}) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link(Job) ->
    gen_fsm:start_link(?MODULE, Job, []).

node_execution_state_updated(JobId, NodeRef, NewState) ->
    lager:info("---------> job:node_execution_state_updated(~p, ~p -> ~p)", [JobId, NodeRef, NewState]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_all_state_event(Pid, {node_execution_state_updated,NodeRef,NewState}).

node_execution_finished(JobId, NodeRef, FinishedReason) ->
    lager:info("---------> job:node_execution_finished(~p, ~p (~p))", [JobId, NodeRef, FinishedReason]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_all_state_event(Pid, {node_execution_finished,NodeRef,FinishedReason}).

get_job_state(JobId) ->
    lager:info("---------> job:get_job_state(~p)", [JobId]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:sync_send_all_state_event(Pid, {get_job_status}).


%
% Init is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
-spec init(#pushy_job{}) ->
    {'ok', 'voting', #state{}} |
    {'stop', 'shutdown', #state{}}.
init(#pushy_job{id = JobId} = Job) ->
    State = #state{job = Job},
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, JobId}),
        {next_state, voting, State2} = set_state(voting, State),
        {ok, voting, State2}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            lager:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [JobId,self(), gproc:lookup_pid({n,l,JobId}) ]),
            {stop, shutdown, State}
    end.

%
% Events
%

-spec handle_event(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_event({node_execution_state_updated,NodeRef,NodeState},
    StateName, State) ->
    update_node_execution_state(NodeRef,NodeState,undefined,StateName,State);
handle_event({node_execution_finished,NodeRef,FinishedReason}, StateName, State) ->
    update_node_execution_state(NodeRef,finished,FinishedReason,StateName,State);
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), job_status(), #state{}) ->
        {'reply', 'ok', job_status(), #state{}}.
handle_sync_event({get_job_status}, _From, StateName, #state{job = Job} = State) ->
    {reply, Job, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

-spec handle_info(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_info(_Info, StateName, State) ->
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
detect_aggregate_state_change(voting, State) ->
    {New, _Ready, _Running, Finished} = count_nodes(State),
    if
        % If any nodes are finished, quorum fails.
        Finished > 0 -> set_state(finished, quorum_failed, State);
        % If any nodes are new, we're not yet ready.
        New > 0 -> {next_state, voting, State};
        % Otherwise, all remaining nodes are ready/running, and we can proceed.
        true -> set_state(running, State)
    end;
detect_aggregate_state_change(running, State) ->
    % If any node is unfinished, we are unfinished.
    {New, Ready, Running, _Finished} = count_nodes(State),
    if
        New+Ready+Running > 0 -> {next_state, running, State};
        true -> set_state(finished, complete, State)
    end;
detect_aggregate_state_change(finished, State) ->
    % Homey don't change state.
    {next_state, finished, State}.

-spec count_nodes(#state{}) ->
    integer().
count_nodes(#state{job = Job}) ->
    lists:foldl(
        fun(#pushy_job_node{status = NodeState},{New, Ready, Running, Finished}) ->
            case NodeState of
                new -> {New+1, Ready, Running, Finished};
                ready -> {New, Ready+1, Running, Finished};
                running -> {New, Ready, Running+1, Finished};
                finished -> {New, Ready, Running, Finished+1}
            end
        end, {0, 0, 0, 0}, Job#pushy_job.job_nodes).

-spec update_node_execution_state(node_ref(), job_node_status(), job_node_finished_reason(), job_status(), #state{}) ->
    {'next_state', job_status(), #state{}}.
update_node_execution_state({_OrgId,NodeName},NodeState,FinishedReason,StateName,
    #state{job=Job}=State) ->
    lager:info("JOB NODE ~p -> ~p (~p)", [NodeName, NodeState, FinishedReason]),
    % TODO handle incorrect org id on node.
    % TODO handle missing node.
    % TODO handle invalid transitions.
    JobNodes = Job#pushy_job.job_nodes,
    JobNodes2 = [
        case JobNodeName of
            NodeName -> JobNode#pushy_job_node{
                status = NodeState,
                finished_reason = FinishedReason
            };
            _ -> JobNode
        end
        || #pushy_job_node{node_name = JobNodeName} = JobNode <- JobNodes
    ],
    Job2 = Job#pushy_job{job_nodes = JobNodes2},
    State2 = State#state{job = Job2},
    detect_aggregate_state_change(StateName,State2).

% Called on transition to a new state
-spec set_state(job_status(), #state{}) ->
    {'next_state', job_status(), #state{}}.
set_state(voting, #state{job = Job} = State) ->
    lager:info("JOB -> voting"),
    send_message_to_all_nodes(<<"job_command">>, State),
    Job2 = Job#pushy_job{status = voting},
    State2 = State#state{job = Job2},
    detect_aggregate_state_change(voting, State2);
set_state(running, #state{job = Job} = State) ->
    lager:info("JOB -> running"),
    % There is really no reason not to TRY sending the message to idle, running
    % or nacked nodes, so we send to everybody.  The worst they could say is no.
    send_message_to_all_nodes(<<"job_execute">>, State),
    Job2 = Job#pushy_job{status = running},
    State2 = State#state{job = Job2},
    detect_aggregate_state_change(running, State2).

set_state(finished, FinishedReason, #state{job = Job} = State) ->
    lager:info("JOB -> finished"),
    % If we finished for any other reason than all tasks completing, we abort all nodes
    % in the job.  Nodes are required to ignore aborts for jobs they aren't running,
    % so this can't cause a problem.
    case FinishedReason of
        complete -> noop;
        _ -> send_message_to_all_nodes(<<"job_abort">>, State)
    end,
    Job2 = Job#pushy_job{status = finished, finished_reason = FinishedReason},
    State2 = State#state{job = Job2},
    {next_state, finished, State2}.

-spec send_message_to_all_nodes(binary(), #state{}) -> 'ok'.
send_message_to_all_nodes(Type, #state{job = Job}) ->
    #pushy_job{id = JobId, org_id = OrgId} = Job,
    NodeNames = [ JobNode#pushy_job_node.node_name || JobNode <- Job#pushy_job.job_nodes ],
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    Message = [
        {type, Type},
        {job_id, JobId},
        {server, list_to_binary(Host)},
        {command, Job#pushy_job.command}
    ],
    pushy_command_switch:send_multi_command(OrgId, NodeNames, jiffy:encode({Message})).
