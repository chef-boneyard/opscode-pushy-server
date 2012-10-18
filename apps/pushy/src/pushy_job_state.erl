%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         node_ack_commit/2,
         node_nack_commit/2,
         node_ack_run/2,
         node_nack_run/2,
         node_complete/2,
         node_aborted/2,
         stop_job/1,
         get_job_state/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% fsm states
-export([voting/2,
         running/2,
         complete/2,
         quorum_failed/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("pushy.hrl").
-include("pushy_sql.hrl").

%% This is Erlang not C/C++
-record(state, {job_host        :: string(),
                job             :: #pushy_job{},
                job_nodes       :: dict(),
                voting_timeout  :: integer(),
                running_timeout :: integer()}).

%%%
%%% External API
%%%
-spec start_link(#pushy_job{}) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Job) ->
    gen_fsm:start_link(?MODULE, Job, []).

-spec node_ack_commit(object_id(), node_ref()) -> ok | not_found.
node_ack_commit(JobId, NodeRef) -> send_node_event(JobId, NodeRef, ack_commit).

-spec node_nack_commit(object_id(), node_ref()) -> ok | not_found.
node_nack_commit(JobId, NodeRef) -> send_node_event(JobId, NodeRef, nack_commit).

-spec node_ack_run(object_id(), node_ref()) -> ok | not_found.
node_ack_run(JobId, NodeRef) -> send_node_event(JobId, NodeRef, ack_run).

-spec node_nack_run(object_id(), node_ref()) -> ok | not_found.
node_nack_run(JobId, NodeRef) -> send_node_event(JobId, NodeRef, nack_run).

-spec node_complete(object_id(), node_ref()) -> ok | not_found.
node_complete(JobId, NodeRef) -> send_node_event(JobId, NodeRef, complete).

-spec node_aborted(object_id(), node_ref()) -> ok | not_found.
node_aborted(JobId, NodeRef) -> send_node_event(JobId, NodeRef, aborted).

get_job_state(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> gen_fsm:sync_send_all_state_event(Pid, get_job_status)
    end.

stop_job(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> gen_fsm:sync_send_all_state_event(Pid, stop_job)
    end.

%%%
%%% Initialization
%%%
-spec init(#pushy_job{}) ->
    {'ok', job_status(), #state{}} |
    {'stop', 'shutdown', #state{}}.
init(#pushy_job{id = JobId, job_nodes = JobNodeList} = Job) ->
    Host = envy:get(pushy, server_name, string),
    case pushy_job_state_sup:register_process(JobId) of
        true ->
            {ok, _} = pushy_object:create_object(create_job, Job, JobId),
            JobNodes = dict:from_list([{{OrgId, NodeName}, JobNode} ||
                                          #pushy_job_node{org_id = OrgId, node_name = NodeName} =
                                              JobNode <- JobNodeList]),
            State = #state{job = Job#pushy_job{job_nodes = undefined},
                           job_nodes = JobNodes,
                           job_host = Host,
                           voting_timeout = envy:get(pushy, voting_timeout, 5000, integer),
                           running_timeout = envy:get(pushy, default_running_timeout, 36000000, integer)},
            listen_for_down_nodes(dict:fetch_keys(JobNodes)),

            lager:info("Job ~p starting '~p' on ~p nodes, with timeout ~p",
                [JobId, Job#pushy_job.command, length(JobNodeList), State#state.running_timeout]),

            % Start voting--if there are no nodes, the job finishes immediately.
            case start_voting(State) of
                {next_state, StateName, State2} -> {ok, StateName, State2};
                {stop, Reason, _State} -> {stop, {shutdown, Reason}}
            end;
        false ->
            {stop, shutdown}
    end.

%%%
%%% Incoming events
%%%

% Nodes can only be new, ready or terminal while we're voting.
voting({ack_commit, NodeRef}, State) ->
    % Node from new -> ready
    State2 = case get_node_state(NodeRef, State) of
        new      -> set_node_state(NodeRef, ready, State);
        ready    -> State;
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_voting(State2);
voting({nack_commit, NodeRef}, State) ->
    % Node from new -> nacked.
    State2 = case get_node_state(NodeRef, State) of
        new      -> set_node_state(NodeRef, nacked, State);
        ready    -> send_to_rehab(NodeRef, unavailable, State);
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_voting(State2);
voting({_, NodeRef}, State) ->
    State2 = case get_node_state(NodeRef, State) of
        new      -> send_to_rehab(NodeRef, unavailable, State);
        ready    -> send_to_rehab(NodeRef, unavailable, State);
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_voting(State2).

% Nodes can never be "new" in voting--only ready, running or terminal.
running({ack_run, NodeRef}, State) ->
    % Node from ready -> running
    State2 = case get_node_state(NodeRef, State) of
        ready    -> set_node_state(NodeRef, running, State);
        running  -> State;
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_running(State2);
running({complete, NodeRef}, State) ->
    State2 = case get_node_state(NodeRef, State) of
        ready    -> send_to_rehab(NodeRef, crashed, State);
        running  -> set_node_state(NodeRef, complete, State);
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_running(State2);
running({_,NodeRef}, State) ->
    State2 = case get_node_state(NodeRef, State) of
        ready    -> send_to_rehab(NodeRef, crashed, State);
        running  -> send_to_rehab(NodeRef, crashed, State);
        terminal -> send_to_rehab(NodeRef, State)
    end,
    maybe_finished_running(State2).

complete({Event,NodeRef}, State) ->
    lager:error("Unexpectedly received node_event message in finished state (~p, ~p)", [Event,NodeRef]),
    {next_state, complete, State}.

quorum_failed({Event,NodeRef}, State) ->
    lager:error("Unexpectedly received node_event message in finished state (~p, ~p)", [Event,NodeRef]),
    {next_state, quorum_failed, State}.

-spec handle_event(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
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
handle_sync_event(stop_job, _From, _StateName, State) ->
    {stop, shutdown, ok, State};
handle_sync_event(Event, From, StateName, State) ->
    lager:error("Unknown message handle_sync_event(~p) from ~p", [Event, From]),
    {next_state, StateName, State}.

-spec handle_info(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_info({down, NodeRef}, StateName, State) ->
    pushy_job_state:StateName({down,NodeRef}, State);
handle_info(voting_timeout, voting,
        #state{job = Job, voting_timeout = VotingTimeout} = State) ->
    lager:info("Timeout occurred during voting on job ~p after ~p milliseconds", [Job#pushy_job.id, VotingTimeout]),
    % Set all nodes that have not responded to the vote, to new, forcing voting to finish
    State2 = send_matching_to_rehab(new, unavailable, State),
    maybe_finished_voting(State2);
% Rather than store and cancel timers, we let them fire and ignore them if we've
% moved on to a new state.
handle_info(voting_timeout, running, State) ->
    {next_state, running, State};
handle_info(running_timeout, running,
        #state{job = Job, running_timeout = RunningTimeout} = State) ->
    lager:info("Timeout occurred while running job ~p after ~p milliseconds", [Job#pushy_job.id, RunningTimeout]),
    State2 = send_matching_to_rehab(ready, aborted, State),
    State3 = send_matching_to_rehab(running, aborted, State2),
    finish_job(timed_out, State3);
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

%%%
%%% Private helper
%%%

get_node_state(NodeRef, #state{job_nodes = JobNodes}) ->
    Node = dict:fetch(NodeRef, JobNodes),
    terminalize(Node#pushy_job_node.status).

set_node_state(NodeRef, NewNodeState, #state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:update(NodeRef, fun(OldPushyJobNode) ->
        case terminalize(OldPushyJobNode#pushy_job_node.status) of
            terminal -> error("Attempt to change node ~p from terminal state ~p to state ~p");
            _ ->
                NewPushyJobNode = OldPushyJobNode#pushy_job_node{status = NewNodeState},
                pushy_sql:update_job_node(NewPushyJobNode),
                NewPushyJobNode
        end
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

send_to_rehab(NodeRef, NewNodeState, State) ->
    State2 = set_node_state(NodeRef, NewNodeState, State),
    send_to_rehab(NodeRef, State2).

send_to_rehab(NodeRef, State) ->
    case get_node_state(NodeRef, State) of
        terminal -> pushy_node_state:rehab(NodeRef);
        _ -> error("Attempt to send node ~p to rehab even though it is in non-terminal state ~p")
    end,
    State.

send_matching_to_rehab(OldNodeState, NewNodeState, #state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:map(fun(_, OldPushyJobNode) ->
        case OldPushyJobNode#pushy_job_node.status of
            OldNodeState ->
                NewPushyJobNode = OldPushyJobNode#pushy_job_node{status = NewNodeState},
                pushy_sql:update_job_node(NewPushyJobNode),
                pushy_node_state:rehab({NewPushyJobNode#pushy_job_node.org_id,
                                        NewPushyJobNode#pushy_job_node.node_name}),
                NewPushyJobNode;
           _ -> OldPushyJobNode
        end
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

maybe_finished_voting(#state{job_nodes = JobNodes} = State) ->
    case count_nodes_in_state([new], State) of
        0 ->
            NumNodes = dict:size(JobNodes),
            case count_nodes_in_state([ready], State) of
                NumNodes -> start_running(State);
                _ ->
                    State2 = send_matching_to_rehab(ready, was_ready, State),
                    finish_job(quorum_failed, State2)
            end;
        _ -> {next_state, voting, State}
    end.

maybe_finished_running(State) ->
    case count_nodes_in_state([ready, running], State) of
        0 -> finish_job(complete, State);
        _ -> {next_state, running, State}
    end.

start_voting(#state{job = Job, voting_timeout = VotingTimeout} = State) ->
    lager:info("Job ~p -> voting", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = voting},
    State2 = State#state{job = Job2},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    send_command_to_all(<<"commit">>, State2),
    {ok, _} = timer:send_after(VotingTimeout, voting_timeout),
    maybe_finished_voting(State2).

start_running(#state{job = Job, running_timeout = RunningTimeout} = State) ->
    lager:info("Job ~p -> running", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = running},
    State2 = State#state{job = Job2},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    send_command_to_all(<<"run">>, State2),
    {ok, _} = timer:send_after(RunningTimeout, running_timeout),
    maybe_finished_running(State2).

finish_job(Reason, #state{job = Job} = State) ->
    lager:info("Job ~p -> ~p", [Job#pushy_job.id, Reason]),
    Job2 = Job#pushy_job{status = Reason},
    State2 = State#state{job = Job2},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    {stop, {shutdown, Reason}, State2}.

count_nodes_in_state(NodeStates, #state{job_nodes = JobNodes}) ->
    dict:fold(
        fun(_Key, NodeState, Count) ->
            case lists:member(NodeState#pushy_job_node.status, NodeStates) of
                true -> Count + 1;
                _ -> Count
            end
        end, 0, JobNodes).

listen_for_down_nodes([]) -> ok;
listen_for_down_nodes([NodeRef|JobNodes]) ->
    pushy_node_state:start_watching(NodeRef),
    case pushy_node_state:current_state(NodeRef) of
        down -> gen_fsm:send_event(self(), {down, NodeRef});
        _ -> ok
    end,
    listen_for_down_nodes(JobNodes).

-spec send_command_to_all(binary(), #state{}) -> 'ok'.
send_command_to_all(Type, #state{job_host=Host, job = Job, job_nodes = JobNodes}) ->
    lager:info("Sending ~p to all nodes", [Type]),
    Message = [{type, Type},
               {job_id, Job#pushy_job.id},
               {server, list_to_binary(Host)},
               {command, Job#pushy_job.command}],
    NodeRefs = dict:fetch_keys(JobNodes),
    pushy_command_switch:send_command(NodeRefs, {Message}).

-spec send_node_event(object_id(), node_ref(), job_event()) -> ok | not_found.
send_node_event(JobId, NodeRef, Event) ->
    lager:info("---------> job:node_event(~p, ~p, ~p)", [JobId, NodeRef, Event]),
    case pushy_job_state_sup:get_process(JobId) of
        Pid when is_pid(Pid) ->
            gen_fsm:send_event(Pid, {Event, NodeRef});
        not_found ->
            pushy_node_state:rehab(NodeRef),
            not_found
    end.

% Used to collapse terminal states (which we always treat the same way) to a
% more matchable form.
terminalize(complete) -> terminal;
terminalize(aborted) -> terminal;
terminalize(unavailable) -> terminal;
terminalize(nacked) -> terminal;
terminalize(crashed) -> terminal;
terminalize(was_ready) -> terminal;
terminalize(timed_out) -> terminal;
terminalize(new) -> new;
terminalize(ready) -> ready;
terminalize(running) -> running.
