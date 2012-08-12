%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_execution_state).

-behaviour(gen_fsm).

%% API
-export([start_link/2,
         set_state/4,
         finished/3]).

%% gen_fsm callbacks
-export([init/1,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include_lib("eunit/include/eunit.hrl").

-type org_id() :: binary().
-type node_name() :: binary().
-type job_id() :: binary().
-type possible_states() :: 'idle' | 'ready' | 'running'.

-record(state,
        {
            org_id    :: org_id(),
            node_name :: node_name(),
            job_id    :: job_id()
        }).

%%%
%%% External API
%%%
-spec start_link(org_id(), node_name()) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link(OrgId, NodeName) ->
    gen_fsm:start_link(?MODULE, {OrgId, NodeName}, []).

set_state(OrgId, NodeName, State, JobId) ->
    lager:info("---> node:set_state(~p -> ~p (~p))", [NodeName, State, JobId]),
    Pid = pushy_node_execution_state_sup:get_process(OrgId, NodeName),
    gen_fsm:send_all_state_event(Pid, {set_state, State, JobId}).

finished(OrgId, NodeName, JobId) ->
    lager:info("---> node:finished(~p (~p))", [NodeName, JobId]),
    Pid = pushy_node_execution_state_sup:get_process(OrgId, NodeName),
    gen_fsm:send_all_state_event(Pid, {finished, JobId}).

%
% gen_fsm
%

%
% This is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
-spec init({org_id(), node_name()}) ->
    {'ok', 'idle', #state{}} |
    {'stop', 'shutdown', #state{}}.
init({OrgId, NodeName}) ->
    lager:info("pushy_node_execution_state:init(~p, ~p)", [OrgId, NodeName]),
    State = #state{
                org_id = OrgId,
                node_name = NodeName
            },
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, {node_execution_state,OrgId,NodeName}}),
        {next_state, idle, State2} = set_state(idle, undefined, State),
        {ok, idle, State2}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            lager:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [{OrgId,NodeName},self(), gproc:lookup_pid({n,l,{node_execution_state,OrgId,NodeName}}) ]),
            {stop, shutdown, State}
    end.

-spec handle_event(any(), possible_states(), #state{}) ->
        {'next_state', possible_states(), #state{}}.
handle_event({set_state, NewState, JobId}, _StateName, State) ->
    set_state(NewState, JobId, State);
handle_event({finished, JobId}, _StateName, #state{
        job_id = OldJobId,
        org_id = OrgId,
        node_name = NodeName} = State) ->
    pushy_job_state:node_execution_finished(JobId, OrgId, NodeName, <<"execution complete">>),
    % Clear out the job ID if it's the same as the finished one, so that we
    % don't send a "something unexpected happened" message to it when we move
    % to idle.
    State2 = case OldJobId of
        JobId -> State#state{job_id = undefined};
        _ -> State
    end,
    set_state(idle, undefined, State2);
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), possible_states(), #state{}) ->
        {'reply', 'ok', possible_states(), #state{}}.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

-spec handle_info(any(), possible_states(), #state{}) ->
        {'next_state', possible_states(), #state{}}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(any(), possible_states(), #state{}) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(any(), possible_states(), #state{}, any()) ->
        {'ok', possible_states(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%
% PRIVATE
%
set_state(NewState, JobId, #state{org_id = OrgId, node_name = NodeName, job_id = OldJobId } = State) ->
    lager:info("NODE STATUS ~p -> ~p", [NodeName,NewState]),
    % Notify the old job (if different).
    case OldJobId of
        JobId -> noop;
        undefined -> noop;
        _ -> pushy_job_state:node_execution_finished(JobId, OrgId, NodeName, <<"missed complete message">>)
    end,
    % Notify the new job (if any).
    case JobId of
        undefined -> noop;
        _ -> pushy_job_state:node_execution_state_updated(JobId, OrgId, NodeName, NewState)
    end,
    State2 = State#state{job_id = JobId},
    {next_state, NewState, State2}.
