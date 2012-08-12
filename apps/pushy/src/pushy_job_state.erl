%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/4,
         node_execution_state_updated/4,
         node_execution_finished/4,
         get_job_state/1]).

%% States
-export([initializing/2]).

%% gen_fsm callbacks
-export([init/1,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pushy_sql.hrl").

-type job_id() :: binary().
-type node_name() :: binary().
-type org_id() :: binary().
-type job_state() :: 'initializing' | 'voting'.
-type node_execution_state() :: 'undefined' | 'ready' | 'running' | 'finished'.

-record(state,
        {
            job_id      :: job_id(),
            org_id      :: org_id(),
            command     :: binary(),
            node_states :: any() % TODO table type?  node_name -> #node_state{}
        }).

-record(node_state,
        {
            state :: 'up' | 'down' | undefined, % TODO get this elsewhere
            execution_state :: 'ready' | 'running' | 'finished' | undefined,
            finished_reason :: binary() | undefined
        }).

%%%
%%% External API
%%%
-spec start_link(job_id(), org_id(), binary(), [node_name()]) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link(JobId, OrgId, Command, NodeNames) ->
    gen_fsm:start_link(?MODULE, {JobId, OrgId, Command, NodeNames}, []).

node_execution_state_updated(JobId, OrgId, NodeName, NewState) ->
    lager:info("---------> job:node_execution_state_updated(~p, ~p -> ~p)", [JobId, NodeName, NewState]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_all_state_event(Pid, {node_execution_state_updated,OrgId,NodeName,NewState}).

node_execution_finished(JobId, OrgId, NodeName, FinishedReason) ->
    lager:info("---------> job:node_execution_finished(~p, ~p (~p))", [JobId, NodeName, FinishedReason]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_all_state_event(Pid, {node_execution_finished,OrgId,NodeName,FinishedReason}).

get_job_state(JobId) ->
    lager:info("---------> job:get_job_state(~p)", [JobId]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:sync_send_all_state_event(Pid, {get_job_state}).


%
% Init is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
-spec init({job_id(), org_id(), binary(), [node_name()]}) ->
    {'ok', 'initializing', #state{}, 0} |
    {'stop', 'shutdown', #state{}}.
init({JobId, OrgId, Command, NodeNames}) ->
    lager:info("pushy_job_state:init(~p, ~p, ~p, ~p)", [JobId, OrgId, Command, NodeNames]),
    State = #state{
                job_id = JobId,
                org_id = OrgId,
                command = Command,
                node_states = dict:from_list([
                    {NodeName,#node_state{}} || NodeName <- NodeNames
                ])
            },
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, JobId}),
        {ok, initializing, State, 0}
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

-spec initializing(any(), #state{}) ->
                    {'next_state', 'voting', #state{}}.
% Lower half of initialization; we have more time for complex work here.
initializing(timeout, State) ->
    lager:info("pushy_job_state:initializing(timeout, ~p)", [State]),
    set_state(voting, State);
initializing(Event, State) ->
    % Resend so it reaches us after initializing
    % TODO could cause out of order message processing if messages show up
    % while we're initializing
    gen_fsm:send_event(self(), Event),
    % Stay in current state
    {next_state, initializing, State}.

-spec handle_event(any(), job_state(), #state{}) ->
        {'next_state', job_state(), #state{}}.
handle_event({node_execution_state_updated,OrgId,NodeName,NodeState},
    StateName, State) ->
    update_node_execution_state(OrgId,NodeName,NodeState,undefined,StateName,State);
handle_event({node_execution_finished,OrgId,NodeName,FinishedReason}, StateName, State) ->
    update_node_execution_state(OrgId,NodeName,finished,FinishedReason,StateName,State);
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), job_state(), #state{}) ->
        {'reply', 'ok', job_state(), #state{}}.
handle_sync_event({get_job_state}, _From, StateName, State) ->
    {reply, to_sql(StateName, State), StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

-spec handle_info(any(), job_state(), #state{}) ->
        {'next_state', job_state(), #state{}}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(any(), job_state(), #state{}) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(any(), job_state(), #state{}, any()) ->
        {'ok', job_state(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%
% PRIVATE
%
% This is called to check whether we are ready to proceed to a different state
% or not.
-spec detect_aggregate_state_change(job_state(), #state{}) ->
    {'next_state', job_state(), #state{}}.
detect_aggregate_state_change(voting, State) ->
    % We don't yet handle nacks or quorum, so we just check whether any nodes
    % are not yet associated with the job.
    case count_nodes_in_state([undefined], State) of
        0 -> set_state(running, State);
        _ -> {next_state, voting, State}
    end;
detect_aggregate_state_change(running, State) ->
    % If any node is unfinished, we are unfinished.
    case count_nodes_in_state([undefined, ready, running], State) of
        0 -> set_state(finished, State);
        _ -> {next_state, running, State}
    end;
detect_aggregate_state_change(finished, State) ->
    % Homey don't change state.
    {next_state, finished, State}.

-spec count_nodes_in_state([node_execution_state()], #state{}) ->
    integer().
count_nodes_in_state(ExpectedStates, #state{node_states = NodeStates}) ->
    dict:fold(
        fun(_,#node_state{execution_state = ExecutionState},AccIn) ->
            AccIn + case lists:member(ExecutionState, ExpectedStates) of
                true -> 1;
                _ -> 0
            end
        end, 0, NodeStates).

-spec update_node_execution_state(org_id(), node_name(), node_execution_state(), binary(), job_state(), #state{}) ->
    {'next_state', job_state(), #state{}}.
update_node_execution_state(_OrgId,NodeName,NodeState,FinishedReason,StateName,
    #state{node_states=NodeStates}=State) ->
    % TODO handle incorrect org id on node.
    % TODO handle missing node.
    % TODO handle invalid transitions.
    OldNodeState = dict:fetch(NodeName,NodeStates),
    NewNodeState = OldNodeState#node_state{
        execution_state = NodeState,
        finished_reason = FinishedReason
    },
    NodeStates2 = dict:store(NodeName, NewNodeState, NodeStates),
    State2 = State#state{node_states = NodeStates2},
    detect_aggregate_state_change(StateName,State2).

% Called on transition to a new state
-spec set_state(job_state(), #state{}) ->
    {'next_state', job_state(), #state{}}.
set_state(initializing, State) ->
    lager:info("JOB -> initializing"),
    {next_state, initializing, State};
set_state(voting, #state{command = Command} = State) ->
    lager:info("JOB -> voting"),
    send_message_to_all_nodes(<<"job_command">>, [{command, Command}], State),
    detect_aggregate_state_change(voting, State);
set_state(running, State) ->
    lager:info("JOB -> running"),
    % There is really no reason not to TRY sending the message to idle, running
    % or nacked nodes, so we send to everybody.  The worst they could say is no.
    send_message_to_all_nodes(<<"job_execute">>, [], State),
    detect_aggregate_state_change(running, State);
set_state(finished, State) ->
    lager:info("JOB -> finished"),
    {next_state, finished, State}.

-spec send_message_to_all_nodes(binary(), list({atom(), any()}), #state{}) -> 'ok'.
send_message_to_all_nodes(Type,
                          ExtraData,
                          #state{job_id = JobId,
                                 org_id = OrgId,
                                 node_states = NodeStates}) ->
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    Message = [
        {type, Type},
        {job_id, JobId},
        {server, list_to_binary(Host)}
    ] ++ ExtraData,
    pushy_command_switch:send_multi_command(OrgId, dict:fetch_keys(NodeStates), jiffy:encode({Message})).

to_sql(StateName, State) ->
    JobNodes = [
        #pushy_job_node{
            job_id = State#state.job_id,
            org_id = State#state.org_id,
            node_name = NodeName,
            status = NodeState#node_state.execution_state,
            finished_reason = NodeState#node_state.finished_reason
        }
        || {NodeName, NodeState} <- dict:to_list(State#state.node_states)
    ],
    #pushy_job{
        id = State#state.job_id,
        org_id = State#state.org_id,
        command = State#state.command,
        status = StateName,
        job_nodes = JobNodes
    }.
