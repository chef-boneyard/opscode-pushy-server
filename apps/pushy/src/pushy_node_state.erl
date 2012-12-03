%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @copyright 2012 Opscode Inc.
%% @end

%%
%% @doc simple FSM for tracking node heartbeats and thus up/down status
%%

-module(pushy_node_state).

-behaviour(gen_fsm).

-include("pushy.hrl").
-include("pushy_sql.hrl").
-include_lib("pushy_common/include/pushy_metrics.hrl").
-include_lib("pushy_common/include/pushy_messaging.hrl").

%% API
-export([start_link/2,
         recv_msg/1,
         send_msg/2,
         send_msg/3,
         heartbeat/1,
         status/1,
         watch/1,
         aborted/1,
         rehab/1]).

%% States
-export([idle/2,
         running/2,
         rehab/2]).

-record(state, {node_ref              :: node_ref(),
                node_addr             :: node_addr(),
                heartbeats = 1        :: pos_integer(),
                job                   :: any(),
                watchers = [],
                state_timer
               }).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-spec start_link(node_ref(), node_addr()) -> ok.
start_link(NodeRef, NodeAddr) ->
    gen_fsm:start_link(?MODULE, [NodeRef, NodeAddr], []).

heartbeat(NodeRef) ->
    send_info(NodeRef, heartbeat),
    ok.
recv_msg(Message) ->
    dispatch_raw_message(Message).

send_msg(NodeRef, Message) ->
    send_msg(NodeRef, hmac_sha256, Message).

send_msg(NodeRef, Method, Message) when is_tuple(NodeRef) ->
    send_info(NodeRef, {send_message, Method, Message});
send_msg(NodeRefs, Method, Message) ->
    [ {NodeRef, send_msg(NodeRef, Method, Message) } || NodeRef <- NodeRefs].

status(NodeRef) ->
    case call(NodeRef, current_state) of
        undefined ->
            {offline, {unavailable, none}};
        CurrentState ->
            eval_state(CurrentState)
    end.

watch(NodeRef) ->
    case call(NodeRef, {watch, self()}) of
        ok ->
            ok;
        Error ->
            Error
    end.

aborted(NodeRef) ->
    case cast(NodeRef, aborted) of
        ok ->
            ok;
        Error ->
            Error
    end.

rehab(NodeRef) ->
    case cast(NodeRef, rehab) of
        ok ->
            ok;
        Error ->
            Error
    end.


init([NodeRef, NodeAddr]) ->
    State = #state{node_ref = NodeRef, node_addr = NodeAddr},
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    GprocAddr = pushy_node_state_sup:mk_gproc_addr(NodeAddr),
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, GprocName}),
        true = gproc:reg({n, l, GprocAddr}),
        State1 = force_abort(State),
        pushy_node_status_updater:create(NodeRef, ?POC_ACTOR_ID, shutdown),
        {ok, state_transition(init, rehab, State1), State1}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            lager:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [NodeRef,self(), gproc:lookup_pid({n,l,GprocName}) ]),
            {stop, state_transition(init, shutdown, State), State}
    end.

rehab(aborted, #state{state_timer=TRef}=State) ->
    timer:cancel(TRef),
    {next_state, state_transition(rehab, idle, State), State};
rehab(Message, #state{node_ref=NodeRef}=State) ->
    lager:info("~p in rehab. Ignoring message: ~p~n", [NodeRef, Message]),
    {next_state, rehab, State}.

idle(rehab, State) ->
    force_abort(State),
    {next_state, state_transition(idle, rehab, State), State};
idle({job, Job}, State) ->
    {next_state, state_transition(idle, running, State), State#state{job=Job}};
idle(aborted, State) ->
    {next_state, idle, State}.

running(aborted, #state{node_ref=NodeRef}=State) ->
    lager:info("~p aborted during job.~n", [NodeRef]),
    State1 = State#state{job=undefined},
    {next_state, state_transition(running, idle, State1), State1};
running({complete, Job}, #state{job=Job, node_ref=NodeRef}=State) ->
    lager:info("~p completed job.~n", [NodeRef]),
    State1 = State#state{job=undefined},
    {next_state, state_transition(running, idle, State1), State1}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({watch, WatcherPid}, _From, StateName, #state{watchers=Watchers}=State) ->
    MRef = erlang:monitor(process, WatcherPid),
    Watcher = {WatcherPid, MRef},
    {reply, ok, StateName, State#state{watchers=[Watcher|Watchers]}};
handle_sync_event({unwatch, WatcherPid}, _From, StateName, #state{watchers=Watchers}=State) ->
    case lists:keytake(WatcherPid, 1, Watchers) of
        false ->
            {reply, ok, StateName, State};
        {value, {WatcherPid, MRef}, Watchers1} ->
            erlang:demonitor(MRef, [flush]),
            {reply, ok, StateName, State#state{watchers=Watchers1}}
    end;
handle_sync_event(current_state, _From, StateName, #state{job=Job}=State) ->
    {reply, {StateName, Job}, StateName, State}.

handle_info(heartbeat, CurrentState, State) ->
    case pushy_node_stats:heartbeat(self()) of
        ok -> {next_state, CurrentState, State};
        should_die -> {stop, state_transition(CurrentState, shutdown, State), State}
    end;
handle_info(should_die, CurrentState, State) ->
    {stop, state_transition(CurrentState, shutdown, State), State};
handle_info(rehab_again, rehab, State) ->
    State1 = force_abort(State),
    {next_state, rehab, State1};
handle_info({send_message, Method, Message}, CurrentState, State) ->
    State1 = do_send(State, Method, Message),
    {next_state, CurrentState, State1};
handle_info({raw_message, Message}, CurrentState, State) ->
    maybe_process_and_dispatch_message(CurrentState, State, Message);
handle_info({'DOWN', _MRef, _Type, Pid, _Reason}, StateName, #state{watchers=Watchers}=State) ->
    case lists:keytake(Pid, 1, Watchers) of
        false ->
            {next_state, StateName, State};
        {value, _, Watchers1} ->
            {next_state, StateName, State#state{watchers=Watchers1}}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
eval_state({idle, undefined}) ->
    {online, {available, none}};
eval_state({rehab, undefined}) ->
    {online, {unavailable, none}};
eval_state({running, Job}) ->
    {online, {unavailable, Job}}.

rehab_interval() ->
    envy:get(pushy, rehab_timer, 1000, integer).

call(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            gen_fsm:sync_send_all_state_event(Pid, Message, infinity);
        Error ->
            Error
    end.

cast(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            gen_fsm:send_event(Pid, Message);
        Error ->
            Error
    end.

send_info(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            Pid ! Message;
        Error ->
            Error
    end.

force_abort(State) ->
    Message = {[{type, abort}]},
    State1 = do_send(State, Message),
    TRef = timer:send_after(rehab_interval(), rehab_again),
    State1#state{state_timer=TRef}.

state_transition(Current, New, #state{node_ref=NodeRef, watchers=Watchers}) ->
    lager:debug("~p transitioning from ~p to ~p~n", [NodeRef, Current, New]),
    pushy_node_status_updater:update(NodeRef, ?POC_ACTOR_ID, New),
    notify_watchers(Watchers, NodeRef, Current, New),
    New.

notify_watchers([], _NodeRef, _Current, _New) ->
    ok;
notify_watchers(Watchers, NodeRef, Current, New) ->
    F = fun(Watcher) -> Watcher ! {state_change, NodeRef, Current, New} end,
    [F(Watcher) || {Watcher, _Monitor} <- Watchers].


%%
%% Message processing and parsing code; this executes in the caller's context
%%
dispatch_raw_message([Addr, _Header, Body] = Message) ->
    Pid = case pushy_node_state_sup:get_process(Addr) of
              P when is_pid(P) ->
                  P;
              _Error ->
                  %% Nothing with that address found; we need to parse the body of the
                  %% message to figure out where to route it...
                  %%
                  %% NOTE: We need to use some care when creating FSMs. Ideally we would
                  %% verify the message before we create the FSM. However, this adds some
                  %% delay to the node startup path, and serializes things in an unpleasant
                  %% way. Instead we speculatively create a new FSM for the node_ref/addr
                  %% pair if there aren't any FSMs registered for either.
                  %%
                  %% So this is only an issue if there is no registered FSM for this
                  %% node_ref or address.
                  %%
                  %% If we get spoofed into creating an FSM, it will simply parse and drop
                  %% spoofed packets, and eventually go down because it hasn't received a
                  %% valid heartbeat. If the real node attempts to connect, it will cause
                  %% the FSM to change its registered address, and the spoofed packets
                  %% will continue to be rejected.
                  %%
                  %% The main threat is some DOS attack where we get tricked into creating
                  %% large numbers of FSMs, which would consume resources.
                  %%
                  %% One defense would be to require that we have a hmac key assigned for
                  %% this node: that would restrict the number of spoofable FSM's to the set
                  %% of nodes that have already made a validated call to the configuration
                  %% endpoint. A further refinement would be to make the node ref cheaper to
                  %% parse and extract (bypassing json perhaps?)
                  EJSon = jiffy:decode(Body),
                  NodeRef = get_node_ref(EJSon),
                  lager:info("No addr ~s for msg: ~p~n", [pushy_tools:bin_to_hex(Addr), NodeRef]),
                  pushy_node_state_sup:get_or_create_process(NodeRef, Addr)
          end,
    Pid ! {raw_message, Message }.

%%
%% This occurs in the fsm context
%%
maybe_process_and_dispatch_message(CurrentState, State, Message) ->
    case process_and_dispatch_message(Message, State) of
        {ok, State1} -> {next_state, CurrentState, State1};
        {should_die, State1} -> {stop, state_transition(CurrentState, shutdown, State1), State1}
    end.

process_and_dispatch_message([Address, Header, Body], State) ->
    KeyFetch = fun key_fetch/2,
    State1 = try ?TIME_IT(pushy_messaging, parse_message, (Address, Header, Body, KeyFetch)) of
                 {ok, #pushy_message{} = Msg} ->
                     {ok, process_message(State, Msg)};
                 {error, #pushy_message{validated=bad_sig}} ->
                     lager:error("Command message failed verification: header=~s", [Header]),
                     {ok, State}
             catch
                 error:Error ->
                     Stack = erlang:get_stacktrace(),
                     lager:error("Command message parser failed horribly: header=~p~nstack~p~s", [Error, Stack]),
                     {ok, State};
                 Error ->
                     Stack = erlang:get_stacktrace(),
                     lager:error("Command message parser failed horribly: header=~p~nstack~p~s", [Error, Stack]),
                     {ok, State}
             end,
    State1.


process_message(#state{node_ref=NodeRef, node_addr=CurAddr} = State, #pushy_message{address=NewAddr} = Message)
  when CurAddr =/= NewAddr ->
    %% Our address has changed. By this point we've validated the message, so we can trust the address
    lager:info("Address change for ~p '~s' to '~s'~n",
               [NodeRef, pushy_tools:bin_to_hex(CurAddr),
                pushy_tools:bin_to_hex(NewAddr)]),
    GprocNewAddr = pushy_node_state_sup:mk_gproc_addr(NewAddr),
    gproc:reg({n, l, GprocNewAddr}),
    GprocCurAddr = pushy_node_state_sup:mk_gproc_addr(CurAddr),
    gproc:unreg({n, l, GprocCurAddr}),
    process_message(State#state{node_addr=NewAddr}, Message);
process_message(#state{node_ref=NodeRef, node_addr=Address} = State, #pushy_message{address=Address, body=Data}) ->
    JobId = ej:get({<<"job_id">>}, Data),
    Type = ej:get({<<"type">>}, Data),
    lager:debug("Received message for Node ~p Type ~p (address ~p)",
                [NodeRef, Type, pushy_tools:bin_to_hex(Address)]),
    send_node_event(State, JobId, NodeRef, Type).

-spec send_node_event(#state{}, any(), any(), binary()) -> #state{}.
send_node_event(State, null, NodeRef, <<"heartbeat">>) ->
    lager:debug("Received heartbeat for node ~p with NULL job id", [NodeRef]),
    self() ! heartbeat,
    State;
send_node_event(State, JobId, NodeRef, <<"heartbeat">>) ->
    lager:debug("Received heartbeat for node ~p with job id ~p", [NodeRef, JobId]),
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> pushy_node_state:rehab(NodeRef);
        _ -> noop
    end,
    self() ! heartbeat,
    State;
send_node_event(State, JobId, NodeRef, <<"ack_commit">>) ->
    pushy_job_state:node_ack_commit(JobId, NodeRef),
    State;
send_node_event(State, JobId, NodeRef, <<"nack_commit">>) ->
    pushy_job_state:node_nack_commit(JobId, NodeRef),
    State;
send_node_event(State, JobId, NodeRef, <<"ack_run">>) ->
    pushy_job_state:node_ack_run(JobId, NodeRef),
    State;
send_node_event(State, JobId, NodeRef, <<"nack_run">>) ->
    pushy_job_state:node_nack_run(JobId, NodeRef),
    State;
send_node_event(State, JobId, NodeRef, <<"succeeded">>)->
    pushy_job_state:node_complete(JobId, NodeRef, succeeded),
    State;
send_node_event(State, JobId, NodeRef, <<"failed">>)->
    pushy_job_state:node_complete(JobId, NodeRef, failed),
    State;
send_node_event(State, JobId, _NodeRef, <<"aborted">>)
  when JobId =:= null orelse JobId =:= <<"null">> ->
    gen_fsm:send_event(self(), aborted),
    State;
send_node_event(State, JobId, NodeRef, <<"aborted">>) ->
    gen_fsm:send_event(self(), aborted),
    pushy_job_state:node_aborted(JobId, NodeRef),
    State;
send_node_event(State, JobId, NodeRef, undefined) ->
    lager:error("Status message for job ~p and node ~p was missing type field!~n", [JobId, NodeRef]),
    State;
send_node_event(State, JobId, NodeRef, UnknownType) ->
    lager:error("Status message for job ~p and node ~p had unknown type ~p~n",
                [JobId, NodeRef, UnknownType]),
    State.

get_node_ref(Data) ->
    %% This essentially debug code.
    _ClientName = ej:get({<<"client">>}, Data),
    OrgName  = ej:get({<<"org">>}, Data),
    NodeName = ej:get({<<"node">>}, Data),
    %% TODO: Clean up usage and propagation of org name vs org guid (OC-4351)
    OrgId = pushy_object:fetch_org_id(OrgName),
    {OrgId, NodeName}.


get_key_for_method(hmac_sha256, {_,_} = NodeRef) ->
    {hmac_sha256, Key} = pushy_key_manager:get_key(NodeRef),
    {ok, Key};
get_key_for_method(rsa2048_sha1, _) ->
    chef_keyring:get_key(pushy_priv).

key_fetch(Method, EJson) ->
    NodeRef = get_node_ref(EJson),
    get_key_for_method(Method, NodeRef).

-spec do_send(#state{}, json_term()) -> #state{}.
do_send(State, Message) ->
    do_send(State, hmac_sha256, Message).

-spec do_send(#state{}, atom(), json_term()) -> #state{}.
do_send(#state{node_addr=NodeAddr, node_ref=NodeRef} = State, Method, Message) ->
    {ok, Key} = get_key_for_method(Method, NodeRef),
    Packets = ?TIME_IT(pushy_messaging, make_message, (proto_v2, Method, Key, Message)),
    ok = pushy_command_switch:send([NodeAddr | Packets]),
    State.
