%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2011-2012 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(pushy_job_state).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/2,
         stop_job/1,
         send_node_event/3,
         get_events/2,
         make_job_summary_event/1
        ]).

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
         waiting_around/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("pushy.hrl").
-include("pushy_sql.hrl").
-include("pushy_event.hrl").

-compile([{parse_transform, lager_transform}]).

%% This is Erlang not C/C++
-record(state, {
                job_host        :: binary(),
                job             :: #pushy_job{},
                job_nodes       :: dict:dict(node_ref(), #pushy_job_node{}),
                voting_timeout  :: integer(),
                term_reason     :: atom(),
                next_event_id   :: integer(),
                rev_events      :: [#event{}],
                subscribers     :: [pid()],
                done            :: boolean()
        }).

%%%
%%% External API
%%%
-spec start_link(#pushy_job{}, binary()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Job, Requestor) ->
    gen_fsm:start_link(?MODULE, {Job, Requestor}, []).

stop_job(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> pushy_fsm_utils:safe_sync_send_all_state_event(Pid, stop_job)
    end.

%%%
%%% Initialization
%%%
init({#pushy_job{id = JobId, job_nodes = JobNodeList, opts = Opts} = Job, Requestor}) ->
    Host = list_to_binary(envy:get(pushy, server_name, string)),
    case pushy_job_state_sup:register_process(JobId) of
        true ->
            pushy_job_monitor:monitor_job(JobId, self()),
            {ok, _} = pushy_object:create_object(create_job, Job, JobId),
            JobNodes = dict:from_list([{{OrgId, NodeName}, JobNode} ||
                                          #pushy_job_node{org_id = OrgId, node_name = NodeName} =
                                              JobNode <- JobNodeList]),
            % All jobs are in the same org.. right?
            [#pushy_job_node{org_id = OrgId}|_] = JobNodeList,
            OrgEvents = pushy_org_events_sup:get_or_create_process(OrgId),
            State0 = #state{job = Job#pushy_job{},
                           job_nodes = JobNodes,
                           job_host = Host,
                           voting_timeout = envy:get(pushy, voting_timeout, 60, integer),
                           next_event_id = 1,
                           rev_events = [],
                           subscribers = [OrgEvents],   % Start by subscribing the org to the feed
                           done = false},
            #pushy_job_opts{user = OptUser, dir = OptDir, env = OptEnv,
                            file = OptFile, capture = OptCapture} = Opts,
            % XXX monitor the OrgEvents; update it if need be
            NodeCount = length(JobNodeList),
            State = add_start_event(
                      State0, Job#pushy_job.command, Job#pushy_job.run_timeout,
                      Job#pushy_job.quorum, NodeCount, Requestor, OptUser,
                      OptDir, OptEnv, OptFile, OptCapture),
            listen_for_down_nodes(dict:fetch_keys(JobNodes)),

            lager:info([{job_id,Job#pushy_job.id}],
                        "Job ~p starting '~p' on ~p nodes, with timeout ~ps",
                        [JobId, Job#pushy_job.command, length(JobNodeList), Job#pushy_job.run_timeout]),
            {next_state, StateName, State2} = start_voting(State),
            {ok, StateName, State2};
        false ->
            lager:error("couldn't register job_state process: ~p", [JobId]),
            {stop, shutdown}
    end.

%%%
%%% Incoming events
%%%

% Nodes can only be new, ready or terminal while we're voting.
voting({ack_commit, _Event, NodeRef}, State) ->
    % Node from new -> ready
    {State1, Status} = case get_node_state(NodeRef, State) of
        new      -> {set_node_state(NodeRef, ready, State), success};
        ready    -> {State, unexpected_commit};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_voting}
    end,
    {_, NodeName} = NodeRef,
    State2 = add_quorum_vote_event(State1, NodeName, Status),
    maybe_finished_voting(State2);
voting({nack_commit, _Event, NodeRef}, State) ->
    % Node from new -> nacked.
    {State1, Status} = case get_node_state(NodeRef, State) of
        new      -> {set_node_state(NodeRef, nacked, State), failure};
        ready    -> {send_to_rehab(NodeRef, unavailable, State), lost_availibility};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_voting}
    end,
    {_, NodeName} = NodeRef,
    State2 = add_quorum_vote_event(State1, NodeName, Status),
    maybe_finished_voting(State2);
voting({Response, _Event, NodeRef}, #state{job = Job} = State) ->
    JobId = Job#pushy_job.id,
    {_, NodeName} = NodeRef,
    State1 = case Response of
        down ->
                add_quorum_vote_event(State, NodeName, down);
        _ ->
                lager:error("Job ~p bad response while voting ~p ~p", [JobId, NodeName, Response]),
                State
    end,
    State2 = case get_node_state(NodeRef, State1) of
        new      -> send_to_rehab(NodeRef, unavailable, State1);
        ready    -> send_to_rehab(NodeRef, unavailable, State1);
        terminal -> send_to_rehab(NodeRef, State1)
    end,
    maybe_finished_voting(State2).

% Nodes can never be "new" when running--only ready, running or terminal.
running({ack_run, _Event, NodeRef}, State) ->
    {_, NodeName} = NodeRef,
    % Node from ready -> running
    State2 = case get_node_state(NodeRef, State) of
        ready    ->
                    State1 = add_run_start_event(State, NodeName),
                    set_node_state(NodeRef, running, State1);
        running  -> State;
        terminal ->
                    State1 = add_run_complete_event(State, NodeName, client_died_while_running),
                    send_to_rehab(NodeRef, State1)
    end,
    maybe_finished_running(State2);
running({nack_run, _Event, NodeRef}, State) ->
    % Node from ready -> running
    {State1, Status} = case get_node_state(NodeRef, State) of
        % A client should never send a "nack_run", it turns out; so if we get one, we tell
        % the client to reset.
        ready    -> {send_to_rehab(NodeRef, failed, State), run_nacked};
        running  -> {send_to_rehab(NodeRef, failed, State), run_nacked_while_running};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_running}
    end,
    {_, NodeName} = NodeRef,
    State2 = add_run_complete_event(State1, NodeName, Status),
    maybe_finished_running(State2);
running({succeeded, Event, NodeRef}, State) ->
    JobId = State#state.job#pushy_job.id,
    capture_any_output(JobId, NodeRef, Event),
    {State1, Status} = case get_node_state(NodeRef, State) of
        ready    -> {send_to_rehab(NodeRef, crashed, State), crashed};
        running  -> {set_node_state(NodeRef, succeeded, State), success};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_running}
    end,
    {_, NodeName} = NodeRef,
    State2 = add_run_complete_event(State1, NodeName, Status),
    maybe_finished_running(State2);
running({failed, Event, NodeRef}, State) ->
    JobId = State#state.job#pushy_job.id,
    capture_any_output(JobId, NodeRef, Event),
    {State1, Status} = case get_node_state(NodeRef, State) of
        ready    -> {send_to_rehab(NodeRef, crashed, State), crashed};
        running  -> {set_node_state(NodeRef, failed, State), failure};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_running}
    end,
    {_, NodeName} = NodeRef,
    State2 = add_run_complete_event(State1, NodeName, Status),
    maybe_finished_running(State2);
running({Failure, _Event, NodeRef}, State) ->
    NodeState = get_node_state(NodeRef, State),
    {State1, Status} = case NodeState of
        ready    -> {send_to_rehab(NodeRef, crashed, State), crashed};
        running  -> {send_to_rehab(NodeRef, crashed, State), crashed};
        terminal -> {send_to_rehab(NodeRef, State), client_died_while_running}
    end,
    {_, NodeName} = NodeRef,
    JobId = State#state.job#pushy_job.id,
    lager:info("Job ~p failure type ~p from node ~p in state ~p", [JobId, Failure, NodeName, NodeState]),
    State2 = add_run_complete_event(State1, NodeName, Status),
    maybe_finished_running(State2).

-spec handle_event(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_event(Event, StateName, State) ->
    JobId = State#state.job#pushy_job.id,
    lager:error("Unknown message handle_event(~p) for job ~p", [Event, JobId]),
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), job_status(), #state{}) ->
                               {'next_state', job_status(), #state{}}|
                               {'reply', #pushy_job{}, job_status(), #state{}} |
                               {'stop', 'shutdown', 'ok', #state{}}.
handle_sync_event(get_subscribers, _From, StateName, State) ->
    % Just for unit tests
    {reply, {ok, State#state.subscribers}, StateName, State};
handle_sync_event(stop_job, _From, _StateName, State) ->
    {stop, shutdown, ok, State};
handle_sync_event({get_events, LastEventID}, {Pid, _}, StateName, State) ->
    Response = build_event_response(LastEventID, State),
    State1 = case Response of
                 {true, _} -> State;
                 % Automatically subscribe to events immediately, to avoid race conditions
                 {false, _} -> add_subscriber(Pid, State)
             end,
    {reply, Response, StateName, State1};
handle_sync_event(Event, From, StateName, State) ->
    lager:error("Unknown message handle_sync_event(~p) from ~p", [Event, From]),
    {next_state, StateName, State}.

-spec handle_info(any(), job_status(), #state{}) ->
        {'next_state', job_status(), #state{}}.
handle_info({state_change, NodeRef, _Current, shutdown}, StateName, State) ->
    pushy_job_state:StateName({down, no_event, NodeRef}, State);
handle_info({state_change, _NodeRef, _Old, _New}, StateName, State) ->
    % Ignore any other state-change messages
    {next_state, StateName, State};
handle_info(voting_timeout, voting,
        #state{job = Job, voting_timeout = VotingTimeout} = State) ->
    lager:debug([{job_id,Job#pushy_job.id}],
                "Timeout occurred during voting on job ~p after ~ps", [Job#pushy_job.id, VotingTimeout]),
    State1 = do_on_matching_state(new, fun(N, S) -> add_quorum_vote_event(S, N#pushy_job_node.node_name, voting_timeout) end, State),
    % Set all nodes that have not responded to the vote, to new, forcing voting to finish
    State2 = send_matching_to_rehab(new, unavailable, State1),
    maybe_finished_voting(State2);
% Rather than store and cancel timers, we let them fire and ignore them if we've
% moved on to a new state.
handle_info(voting_timeout, running, State) ->
    {next_state, running, State};
handle_info(running_timeout, running,
        #state{job = Job} = State) ->
    lager:debug([{job_id,Job#pushy_job.id}],
                "Timeout occurred while running job ~p after ~ps", [Job#pushy_job.id, Job#pushy_job.run_timeout]),
    State2 = send_matching_to_rehab(ready, timed_out, State),
    State3 = send_matching_to_rehab(running, timed_out, State2),
    finish_job(timed_out, State3);
handle_info(_, waiting_around, State) ->
    % ignore anything other than the timeout -- if we exited instead of waiting around, these would be
    % ignored anyway.
    {next_state, waiting_around, State};
handle_info(ping, StateName, State) ->
    %?debugFmt("~p: PING", [iolist_to_binary(pushy_event:get_time_as_iso8601(erlang:now()))]),
    % Solely for testing -- force a message to go out so we ca detect if the connection has gone down.
    post_to_subscribers(State, ping),
    {next_state, StateName, State};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, StateName, State) ->
    %?debugFmt("~p: DOWN", [iolist_to_binary(pushy_event:get_time_as_iso8601(erlang:now()))]),
    State1 = remove_subscriber(Pid, State),
    {next_state, StateName, State1};
handle_info(Info, StateName, State) ->
    lager:error("Unknown message handle_info(~p, ~p)", [Info, StateName]),
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
                {ok, 1} = pushy_object:update_object(update_job_node, NewPushyJobNode),
                NewPushyJobNode
        end
    end, JobNodes),
    State#state{job_nodes = JobNodes2}.

send_to_rehab(NodeRef, NewNodeState, State) ->
    State1 = set_node_state(NodeRef, NewNodeState, State),
    send_to_rehab(NodeRef, State1).

send_to_rehab(NodeRef, State) ->
    case get_node_state(NodeRef, State) of
        terminal ->
            pushy_node_state:rehab(NodeRef),
            {_, NodeName} = NodeRef,
            add_rehab_event(State, NodeName);
        _ ->
            error("Attempt to send node ~p to rehab even though it is in non-terminal state ~p"),
            State
    end.

send_matching_to_rehab(OldNodeState, NewNodeState, #state{job_nodes = JobNodes} = State) ->
    JobNodes2 = dict:map(
        fun(_, OldPushyJobNode) ->
            case OldPushyJobNode#pushy_job_node.status of
                OldNodeState ->
                    NewPushyJobNode = OldPushyJobNode#pushy_job_node{status = NewNodeState},
                    {ok, 1} = pushy_object:update_object(update_job_node, NewPushyJobNode),
                    pushy_node_state:rehab({NewPushyJobNode#pushy_job_node.org_id,
                                            NewPushyJobNode#pushy_job_node.node_name}),
                    NewPushyJobNode;
               _ -> OldPushyJobNode
            end
        end, JobNodes),
    State#state{job_nodes = JobNodes2}.

-spec do_on_matching_state(atom(), fun((#pushy_job_node{}, #state{}) -> #state{}), #state{}) -> #state{}.
do_on_matching_state(NodeState, Fun, #state{job_nodes = JobNodes} = State) ->
    Nodes = [N || {_,N} <- dict:to_list(dict:filter(fun(_, Node) -> Node#pushy_job_node.status =:= NodeState end, JobNodes))],
    % "Fun" takes the node and the state, and returns a new state.
    lists:foldl(Fun, State, Nodes).

maybe_finished_voting(#state{job = Job} = State) ->
    case count_nodes_in_state([new], State) of
        0 ->
            QuorumMinimum = Job#pushy_job.quorum,
            case count_nodes_in_state([ready], State) >= QuorumMinimum of
                true ->
                    State1 = add_quorum_succeeded_event(State),
                    start_running(State1);
                _ ->
                    State1 = send_matching_to_rehab(ready, was_ready, State),
                    finish_job(quorum_failed, State1)
            end;
        _ -> {next_state, voting, State}
    end.

maybe_finished_running(State) ->
    case count_nodes_in_state([ready, running], State) of
        0 -> finish_job(complete, State);
        _ -> {next_state, running, State}
    end.

start_voting(#state{job = Job, voting_timeout = VotingTimeout} = State) ->
    lager:debug([{job_id,Job#pushy_job.id}],
                "Job ~p -> voting", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = voting},
    State2 = State#state{job = Job2},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    send_commit(State2),
    {ok, _} = timer:send_after(VotingTimeout*1000, voting_timeout),
    maybe_finished_voting(State2).

start_running(#state{job = Job} = State) ->
    lager:debug([{job_id,Job#pushy_job.id}],
                "Job ~p -> running", [Job#pushy_job.id]),
    Job2 = Job#pushy_job{status = running},
    State2 = State#state{job = Job2},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    send_run(State2),
    {ok, _} = timer:send_after(Job2#pushy_job.run_timeout*1000, running_timeout),
    maybe_finished_running(State2).

finish_job(Reason, #state{job = Job} = State) ->
    % All nodes are guaranteed to be in terminal state by this point, so no
    % nodes need to be sent to rehab.
    lager:info([{job_id,Job#pushy_job.id}],
                "Job ~p completed with state ~p", [Job#pushy_job.id, Reason]),
    State1 = add_job_complete_event(State, Reason),
    post_to_subscribers(State1, done),
    Job2 = Job#pushy_job{status = Reason},
    State2 = State1#state{job = Job2, term_reason = Reason, subscribers = [], done = true},
    pushy_object:update_object(update_job, Job2, Job2#pushy_job.id),
    % Wait around for a while, in case someone wants to get events describing this job
    WaitCompleteTime = envy:get(pushy, wait_complete_time, 5, integer),
    gen_fsm:start_timer(WaitCompleteTime*1000, wait_complete),
    {next_state, waiting_around, State2}.

waiting_around({timeout, _Ref, wait_complete}, #state{job=Job} = State) ->
     lager:info([{job_id,Job#pushy_job.id}],
                "Job ~p completed waiting shutting down", [Job#pushy_job.id]),
    {stop, {shutdown, State#state.term_reason}, State};
waiting_around(_, State) ->
    % ignore anything other than the timeout -- if we exited instead of waiting around, these would be
    % ignored anyway.
    {next_state, waiting_around, State}.

count_nodes_in_state(NodeStates, #state{job_nodes = JobNodes}) ->
    dict:fold(
        fun(_Key, NodeState, Count) ->
            case lists:member(NodeState#pushy_job_node.status, NodeStates) of
                true -> Count + 1;
                _ -> Count
            end
        end, 0, JobNodes).

%% @doc Return the set of NodeRefs for nodes which are in a given set
%% of states.
nodes_in_state(NodeStates, #state{job_nodes = JobNodes}) ->
    dict:fold(
        fun(NodeRef, NodeState, Acc) ->
            case lists:member(NodeState#pushy_job_node.status, NodeStates) of
                true -> [NodeRef | Acc];
                _ -> Acc
            end
        end, [], JobNodes).

listen_for_down_nodes([]) -> ok;
listen_for_down_nodes([NodeRef|JobNodes]) ->
    pushy_node_state:watch(NodeRef),
    case pushy_node_state:status(NodeRef) of
        {_, {unavailable, _}} ->
            gen_fsm:send_event(self(), {down, no_event, NodeRef});
        _ -> ok
    end,
    listen_for_down_nodes(JobNodes).

-spec send_run(#state{}) -> 'ok'.
send_run(#state{job = Job} = State) ->
    lager:debug([{job_id,Job#pushy_job.id}], "Sending run to nodes in ready state"),
    ReadyNodeRefs = nodes_in_state([ready], State),
    send_msg_to_nodes(<<"run">>, [], Job, ReadyNodeRefs).

get_attr_list(_Name, undefined) -> [];
get_attr_list(Name, Val) -> [{Name, Val}].

-spec send_commit(#state{}) -> 'ok'.
send_commit(#state{job_host=Host, job = Job, job_nodes = JobNodes}) ->
    lager:debug([{job_id,Job#pushy_job.id}], "Sending commit to all nodes"),
    NodeRefs = dict:fetch_keys(JobNodes),
    Opts = Job#pushy_job.opts,
    % XX Consider attaching file in separate frame
    #pushy_job_opts{user = User, dir = Dir, env = Env, file = File,
                    capture = Capture} = Opts,
    OptKVs = [{user, User}, {dir, Dir}, {env, Env}, {file, File},
              {capture, Capture}],
    OptsPL = [{K, V} || {K, V} <- OptKVs, V /= undefined],
    Attrs = [{server, Host},
             {command, Job#pushy_job.command}] ++ OptsPL,
    send_msg_to_nodes(<<"commit">>, Attrs, Job, NodeRefs).

send_msg_to_nodes(Type, MoreAttrs, Job, NodeRefs) ->
    Message = [{type, Type}, {job_id, Job#pushy_job.id} | MoreAttrs],
    pushy_node_state:send_msg(NodeRefs, {Message}).

-spec send_node_event(object_id(), node_ref(), job_event()) -> ok | not_found.
send_node_event(JobId, NodeRef, {Type, Event}) ->
    lager:debug("---------> job:node_event(~p, ~p, ~p, ~p)", [JobId, NodeRef, Type, Event]),
    case pushy_job_state_sup:get_process(JobId) of
        Pid when is_pid(Pid) ->
            gen_fsm:send_event(Pid, {Type, Event, NodeRef});
        not_found ->
            not_found
    end.

% Used to collapse terminal states (which we always treat the same way) to a
% more matchable form.
terminalize(succeeded) -> terminal;
terminalize(failed) -> terminal;
terminalize(aborted) -> terminal;
terminalize(unavailable) -> terminal;
terminalize(nacked) -> terminal;
terminalize(crashed) -> terminal;
terminalize(was_ready) -> terminal;
terminalize(timed_out) -> terminal;
terminalize(new) -> new;
terminalize(ready) -> ready;
terminalize(running) -> running.

%% Event-handling
-spec get_events(pid(), string()) -> {boolean(), iolist()}.
get_events(JobPid, LastEventID) ->
    pushy_fsm_utils:safe_sync_send_all_state_event(JobPid, {get_events, LastEventID}).

%% event utility functions
-type status() :: atom() | binary() | {string(), [any()]} | iolist().

build_event_response(LastEventID, State = #state{done = Done}) ->
    RevEvs = case LastEventID of
                 undefined -> State#state.rev_events;
                 % The events are in reverse order, so we want the beginning of the list, not the end.
                 % Happily, this does the right thing if the LastEventID doesn't exist.
                 S -> lists:takewhile(fun(E) -> E#event.id /= list_to_binary(S) end, State#state.rev_events)
             end,
    {Done, lists:reverse(RevEvs)}.

-spec add_event(#state{}, string(), [{binary()|atom(), binary()|non_neg_integer()|boolean()}]) -> #state{}.
add_event(State = #state{next_event_id = Id, rev_events = RevEvents}, EventName, PropList) ->
    IdStr = iolist_to_binary(io_lib:format("job-~B", [Id])),
    Ev = pushy_event:make_event(EventName, IdStr, PropList),
    JobId = State#state.job#pushy_job.id,
    post_to_subscribers(State, {job_ev, JobId, Ev}),
    State#state{next_event_id = Id + 1, rev_events = [Ev | RevEvents]}.

post_to_subscribers(#state{subscribers = Subscribers}, Msg) ->
    lists:foreach(fun(W) -> W ! Msg end, Subscribers).

-spec add_start_event(#state{}, binary(), non_neg_integer(), non_neg_integer(),
                      non_neg_integer(), binary(), binary()|undefined,
                      binary()|undefined, binary()|undefined,
                      binary()|undefined, boolean()|undefined) -> #state{}.
add_start_event(State, Command, RunTimeout, Quorum, NodeCount, User, JobUser, Dir, Env, File, Capture) ->
    JobUserPL = get_attr_list(job_user, JobUser),
    DirPL = get_attr_list(dir, Dir),
    EnvPL = get_attr_list(env, Env),
    FileSpecified = case File of
                        undefined -> undefined;
                        _ -> true
                    end,
    FileSpecifiedPL = get_attr_list(file_specified, FileSpecified),
    CapturePL = get_attr_list(capture, Capture),
    OptPL = JobUserPL ++ DirPL ++ EnvPL ++ FileSpecifiedPL ++ CapturePL,
    add_event(State, "start", [{<<"command">>, Command},
                               {<<"run_timeout">>, RunTimeout},
                               {<<"quorum">>, Quorum},
                               {<<"node_count">>, NodeCount},
                               {<<"user">>, User}] ++ OptPL).

-spec add_quorum_vote_event(#state{}, binary(), status()) -> #state{}.
add_quorum_vote_event(State, Node, Status) ->
    add_event(State, "quorum_vote", [{<<"node">>, Node}, {<<"status">>, status_to_binary(Status)}]).

-spec add_quorum_succeeded_event(#state{}) -> #state{}.
add_quorum_succeeded_event(State) ->
    add_event(State, "quorum_succeeded", []).

-spec add_run_start_event(#state{}, binary()) -> #state{}.
add_run_start_event(State, Node) ->
    add_event(State, "run_start", [{<<"node">>, Node}]).

-spec add_run_complete_event(#state{}, binary(), status()) -> #state{}.
add_run_complete_event(State, Node, Status) ->
    add_event(State, "run_complete", [{<<"node">>, Node}, {<<"status">>, status_to_binary(Status)}]).

-spec add_job_complete_event(#state{}, status()) -> #state{}.
add_job_complete_event(State, Status) ->
    add_event(State, "job_complete", [{<<"status">>, status_to_binary(Status)}]).

-spec add_rehab_event(#state{}, binary()) -> #state{}.
add_rehab_event(State, Node) ->
    add_event(State, "rehab", [{<<"node">>, Node}]).

atom_to_binary(A) -> list_to_binary(atom_to_list(A)).

%status_to_binary(S) when is_binary(S) -> S;
status_to_binary(S) when is_atom(S) -> atom_to_binary(S).
%status_to_binary(S) when is_list(S) -> iolist_to_binary(S);
%status_to_binary({F, A}) when is_list(F) -> iolist_to_binary(io_lib:format(F, A)).

make_job_summary_event(Job) ->
    {PL} = pushy_object:assemble_job_ejson_with_nodes(Job),
    pushy_event:make_event("summary", <<"job-1">>, PL).

add_subscriber(Pid, State = #state{subscribers = Ss}) ->
    % Given how webmachine works, there is no way of knowing that an HTTP request has closed,
    % except that the process went down.  Not a bad backup to watch for the subscriber exit
    % anyway.
    monitor(process, Pid),
    State#state{subscribers = [Pid | Ss]}.

remove_subscriber(Pid, State = #state{subscribers = Ss}) ->
    State#state{subscribers = [P || P <- Ss, P /= Pid]}.

capture_any_output(JobId, NodeRef, Event) ->
    Stdout = ej:get({<<"stdout">>}, Event),
    Stderr = ej:get({<<"stderr">>}, Event),
    {_, NodeName} = NodeRef,
    pushy_sql:insert_job_output(JobId, NodeName, Stdout, Stderr).
