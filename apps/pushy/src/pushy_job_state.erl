%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         node_event/3,
         get_job_state/1]).

%% gen_fsm callbacks
-export([init/1,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4,
     voting/2,
     running/2,
     complete/2,
     quorum_failed/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pushy.hrl").
-include_lib("pushy_sql.hrl").

-record(state,
        {
            job            :: #pushy_job{},
            job_nodes      :: dict() % dict(node_ref(), job_node_status())
        }).

%%%
%%% External API
%%%
-spec start_link(#pushy_job{}) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Job) ->
    gen_fsm:start_link(?MODULE, Job, []).

-spec node_event(object_id(), node_ref(), ack_commit | nack_commit | ack_run | nack_run | complete | aborted | down) -> ok.
node_event(JobId, NodeRef, Event) ->
    lager:info("---------> job:node_event(~p, ~p, ~p)", [JobId, NodeRef, Event]),
    Pid = pushy_job_state_sup:get_process(JobId),
    gen_fsm:send_event(Pid, {Event, NodeRef}).

get_job_state(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> gen_fsm:sync_send_all_state_event(Pid, get_job_status)
    end.

%%%
%%% Initialization
%%%
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
            State = #state{
                job = Job#pushy_job{job_nodes = undefined},
                job_nodes = JobNodes
            },
            % If there are no nodes, the job finishes immediately.
            case dict:size(JobNodes) of
                0 -> finish_job(complete, State);
                _ ->
                    listen_for_down_nodes(dict:fetch_keys(JobNodes)),
                    start_voting(State)
            end;
        false ->
            {stop, shutdown, undefined}
    end.

%%%
%%% Incoming events
%%%

voting({ack_commit, NodeRef}, State) ->
    % Node from new -> ready
    case get_node_state(NodeRef, State) of
        new ->
            State2 = set_node_state(NodeRef, ready, State),
            case count_nodes_in_state([new], State2) of
                0        -> start_running(State2);
                _        -> {next_state, voting, State2}
            end;
        ready ->
            {next_state, voting, State};
        _ ->
            State2 = eject_node(NodeRef, State),
            finish_job(quorum_failed, State2)
    end;
voting({nack_commit, NodeRef}, State) ->
    % Node from new -> nacked.
    State2 = case get_node_state(NodeRef, State) of
        new -> set_node_state(NodeRef, nacked, State);
        _   -> eject_node(NodeRef, State)
    end,
    % Whether it's faulty or not, the job is done (quorum failed).
    finish_job(quorum_failed, State2);
voting({down, NodeRef}, State) ->
    % Node from new -> unavailable.
    State2 = case get_node_state(NodeRef, State) of
        new -> set_node_state(NodeRef, unavailable, State);
        _   -> eject_node(NodeRef, State)
    end,
    finish_job(quorum_failed, State2);
voting({_, NodeRef}, State) ->
    State2 = eject_node(NodeRef, State),
    finish_job(quorum_failed, State2).

running({ack_run, NodeRef}, State) ->
    % Node from ready -> running
    case get_node_state(NodeRef, State) of
        ready ->
            State2 = set_node_state(NodeRef, running, State),
            {next_state, running, State2};
        running -> {next_state, running, State};
        _ ->
            State2 = eject_node(NodeRef, State),
            maybe_finished_running(State2)
    end;
running({complete, NodeRef}, State) ->
    State2 = case get_node_state(NodeRef, State) of
        running -> set_node_state(NodeRef, complete, State);
        _       -> eject_node(NodeRef, State)
    end,
    maybe_finished_running(State2);
running({aborted, NodeRef}, State) ->
    State2 = case get_node_state(NodeRef, State) of
        running -> set_node_state(NodeRef, aborted, State);
        _       -> eject_node(NodeRef, State)
    end,
    maybe_finished_running(State2);
running({_,NodeRef}, State) ->
    State2 = eject_node(NodeRef, State),
    maybe_finished_running(State2).

complete({Event,NodeRef}, State) ->
    lager:error("Unexpectedly received node_event message in finished state (~p, ~p)", [Event,NodeRef]),
    {next_state, complete, State}.

quorum_failed({Event,NodeRef}, State) ->
    lager:error("Unexpectedly received node_event message in finished state (~p, ~p)", [Event,NodeRef]),
    {next_state, quorum_failed, State}.

-spec handle_event(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_event(Event, _StateName, State) ->
    lager:error("Unknown message handle_event(~p)", [Event]),
    finish_job(faulty, State).

-spec handle_sync_event(any(), any(), job_status(), #state{}) ->
        {'reply', 'ok', job_status(), #state{}}.
handle_sync_event(get_job_status, _From, StateName,
        #state{job = Job, job_nodes = JobNodes} = State) ->
    JobNodesList = [ JobNode || {_,JobNode} <- dict:to_list(JobNodes) ],
    Job2 = Job#pushy_job{job_nodes = JobNodesList},
    {reply, Job2, StateName, State};
handle_sync_event(Event, From, _StateName, State) ->
    lager:error("Unknown message handle_sync_event(~p) from ~p", [Event, From]),
    finish_job(faulty, State).

-spec handle_info(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_info({down,NodeRef}, StateName, State) ->
    pushy_job_state:StateName({down,NodeRef}, State);
handle_info(Info, _StateName, State) ->
    lager:error("Unknown message handle_info(~p)", [Info]),
    finish_job(faulty, State).

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
    Node#pushy_job_node.status.

set_node_state(NodeRef, NewNodeState, #state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:update(NodeRef, fun(OldNodeState) ->
        OldNodeState#pushy_job_node{status = NewNodeState}
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

eject_node(NodeRef, #state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:update(NodeRef, fun(OldNodeState) ->
        case OldNodeState#pushy_job_node.status of
            new -> OldNodeState#pushy_job_node{status = faulty};
            ready -> OldNodeState#pushy_job_node{status = faulty};
            running -> OldNodeState#pushy_job_node{status = faulty};
            _ -> OldNodeState
        end
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

eject_all_nodes(#state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:map(fun(_, OldNodeState) ->
        case OldNodeState#pushy_job_node.status of
            new -> OldNodeState#pushy_job_node{status = faulty};
            ready -> OldNodeState#pushy_job_node{status = faulty};
            running -> OldNodeState#pushy_job_node{status = faulty};
            _ -> OldNodeState
        end
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

maybe_finished_running(State) ->
    case count_nodes_in_state([ready, running], State) of
        0 -> finish_job(complete, State);
        _ -> {next_state, running, State}
    end.

start_voting(#state{job = Job} = State) ->
    lager:info("Job ~p -> voting", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = voting},
    State2 = State#state{job = Job2},
    send_command_to_all(<<"job_command">>, State2),
    {ok, voting, State2}.

start_running(#state{job = Job} = State) ->
    lager:info("Job ~p -> running", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = running},
    State2 = State#state{job = Job2},
    send_command_to_all(<<"job_execute">>, State2),
    {next_state, running, State2}.

finish_job(Reason, #state{job = Job} = State) ->
    lager:info("Job ~p -> ~p", [Job#pushy_job.id, Reason]),
    Job2 = Job#pushy_job{status = Reason},
    State2 = State#state{job = Job2},
    State3 = eject_all_nodes(State2),
    {next_state, Reason, State3}.

count_nodes_in_state(NodeStates, #state{job_nodes = JobNodes}) ->
    dict:fold(
        fun(_, NodeState, Count) ->
            case lists:member(NodeState, NodeStates) of
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
send_command_to_all(Type, #state{job = Job, job_nodes = JobNodes}) ->
    lager:info("Sending ~p to all nodes", [Type]),
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    Message = [
        {type, Type},
        {job_id, Job#pushy_job.id},
        {server, list_to_binary(Host)},
        {command, Job#pushy_job.command}
    ],
    NodeRefs = dict:fetch_keys(JobNodes),
    pushy_command_switch:send_multi_command(NodeRefs, jiffy:encode({Message})).
