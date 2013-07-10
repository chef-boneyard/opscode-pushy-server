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

-define(MAX_JOB_ID_LENGTH, 64).

%% API
-export([start_link/3,
         recv_msg/1,
         send_msg/2,
         send_msg/3,
         status/1,
         watch/1,
         rehab/1]).

%% States
-export([idle/2,
         post_init/2,
         rehab/2]).

%% Exposed for testing support
-export([
         heartbeat/2,
         aborted/1
        ]).

-record(state, {node_ref              :: node_ref(),
                node_addr             :: node_addr(),
                heartbeats = 1        :: pos_integer(),
                job                   :: any(),
                sequence_no = 1       :: pos_integer(),
                availability          :: node_availability(),
                watchers = [],
                incarnation_id,
                state_timer
               }).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-spec start_link(node_ref(), node_addr(), binary()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(NodeRef, NodeAddr, IncarnationId) ->
    gen_fsm:start_link(?MODULE, [NodeRef, NodeAddr, IncarnationId], []).

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
        not_found ->
            {offline, {unavailable, none}};
        CurrentState ->
            eval_state(CurrentState)
    end.

-spec watch(NodeRef :: node_ref()) -> term() | not_found.
watch(NodeRef) ->
    call(NodeRef, {watch, self()}).

-spec rehab(NodeRef :: node_ref()) -> ok | undefined.
rehab(NodeRef) ->
    cast(NodeRef, do_rehab).

%% Testing API

-spec heartbeat(NodeRef :: node_ref(),
                IncarnationId :: binary()) -> ok.
heartbeat(NodeRef, IncarnationId) ->
    send_info(NodeRef, {heartbeat, IncarnationId}).

-spec aborted(NodeRef :: node_ref()) -> ok.
aborted(NodeRef) ->
    cast(NodeRef, aborted).


%% Gen_fsm
init([NodeRef, NodeAddr, IncarnationId]) ->
    State = #state{node_ref = NodeRef, node_addr = NodeAddr,
                   incarnation_id = IncarnationId, availability=unavailable},
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    GprocAddr = pushy_node_state_sup:mk_gproc_addr(NodeAddr),
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, GprocName}),
        true = gproc:reg({n, l, GprocAddr}),
        %% We then move to a post_init state that handles the rest of the setup process
        {ok, post_init, State, 0}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            pushy_logger:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [NodeRef,self(), gproc:lookup_pid({n,l,GprocName}) ]),
            {stop, state_transition(init, shutdown, State), State}
    end.

%%
%% Our usage pattern is to create the FSM and immediately send a message (most likely a
%% heartbeat), so this timeout will often never fire because we get a message
%% first. handle_info will get heartbeat messages and resend them, while any others should
%% be ignored, as they aren't relevant in post_init.
%%
post_init(timeout, State) ->
    State1 = force_abort(State),
    {next_state, state_transition(init, rehab, State1), State1};
post_init(Message, #state{node_ref=NodeRef}=State) ->
    pushy_logger:warning("~p in post_init. Ignoring message: ~p~n", [NodeRef, Message]),
    State1 = force_abort(State),
    {next_state, state_transition(init, rehab, State1), State1}.

rehab(aborted, #state{state_timer=TRef}=State) ->
    _Result = timer:cancel(TRef),
    State1 = State#state{availability=available},
    {next_state, state_transition(rehab, idle, State1), State1};
rehab(Message, #state{node_ref=NodeRef}=State) ->
    pushy_logger:debug("~p in rehab. Ignoring message: ~p~n", [NodeRef, Message]),
    {next_state, rehab, State}.

idle(do_rehab, State) ->
    State1 = force_abort(State),
    {next_state, state_transition(idle, rehab, State1), State1};
idle(aborted, State) ->
    {next_state, idle, State}.

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

handle_info(Message, post_init, State) ->
    %% Startup of a node_fsm creates the FSM and immediately in the same process sends a
    %% message. The node FSM never gets the chance to timeout in post_init, since by the time the do loop runs
    %% there's a message waiting. So we do the post_init work here, and resend the message to our selves.
    State1 = force_abort(State),
    send_info(self(), Message),
    {next_state, rehab, State1};
handle_info({heartbeat, IncarnationId}, CurrentState,
        #state{incarnation_id = OrigIncarnationId} = State) ->
    if
        IncarnationId =/= OrigIncarnationId ->
            {stop, state_transition(CurrentState, shutdown, State), State};
        true ->
            case pushy_node_stats:heartbeat(self()) of
                ok -> {next_state, CurrentState, State};
                should_die -> {stop, state_transition(CurrentState, shutdown, State), State}
            end
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

terminate(_Reason, _StateName, #state{node_ref = NodeRef}) ->
    pushy_logger:debug("Shutting Down: ~p~n", [NodeRef]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
eval_state({idle, undefined}) ->
    {online, {available, none}};
eval_state({post_init, undefined}) ->
    {online, {unavailable, none}};
eval_state({rehab, undefined}) ->
    {online, {unavailable, none}}.

rehab_interval() ->
    envy:get(pushy, rehab_timer, 1000, integer).

-spec call(NodeRef :: node_ref(),
           Message :: any()) -> term() | not_found.
call(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            pushy_fsm_utils:safe_sync_send_all_state_event(Pid, Message);
        undefined ->
            not_found
    end.

-spec cast(NodeRef :: node_ref(),
           Message :: any()) -> ok | undefined.
cast(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            gen_fsm:send_event(Pid, Message);
        undefined ->
            undefined
    end.

-spec send_info(pid() | node_ref(), Message :: any()) -> ok | undefined.
send_info(NodePid, Message) when is_pid(NodePid) ->
    NodePid ! Message;
send_info(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            Pid ! Message;
        undefined ->
            undefined
    end.

force_abort(State) ->
    Message = {[{type, abort}]},
    State1 = do_send(State, Message),
    TRef = timer:send_after(rehab_interval(), rehab_again),
    State1#state{state_timer=TRef}.

state_transition(Current, New,
        #state{node_ref=NodeRef, watchers=Watchers, availability=Availability}) ->
    pushy_logger:debug("~p transitioning from ~p to ~p~n", [NodeRef, Current, New]),
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    gproc:set_value({n, l, GprocName}, Availability),
    notify_watchers(Watchers, NodeRef, Current, New),
    New.

notify_watchers([], _NodeRef, _Current, _New) ->
    ok;
notify_watchers([{Watcher,_Monitor}|Tail], NodeRef, Current, New) ->
    Watcher ! {state_change, NodeRef, Current, New},
    notify_watchers(Tail, NodeRef, Current, New).

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
                  IncarnationId = ej:get({<<"incarnation_id">>}, EJSon),
                  pushy_logger:debug("No addr ~s for msg: ~p~n", [pushy_tools:bin_to_hex(Addr), NodeRef]),
                  pushy_node_state_sup:get_or_create_process(NodeRef, Addr, IncarnationId)
          end,
    send_info(Pid, {raw_message, Message}).

%%
%% This occurs in the fsm context
%%
maybe_process_and_dispatch_message(CurrentState, State, Message) ->
    case process_and_dispatch_message(Message, State) of
        {ok, State1} -> {next_state, CurrentState, State1}
        %% TODO - JC we never get a {should_die, State} right now.  Need to improve logic around
        %% failed decode messages
        %%{should_die, State1} -> {stop, state_transition(CurrentState, shutdown, State1), State1}
    end.

-spec process_and_dispatch_message(Message :: [binary()],
                                   State :: #state{} ) -> {ok, #state{} }.
process_and_dispatch_message([Address, Header, Body], State) ->
    KeyFetch = fun key_fetch/2,
    try ?TIME_IT(pushy_messaging, parse_message, (Address, Header, Body, KeyFetch)) of
        {ok, #pushy_message{} = Msg} ->
            {ok, process_message(State, Msg)};
        {error, #pushy_message{validated=bad_sig}} ->
            pushy_logger:error("Bad signature in message: header=~s, body=~s", [Header, Body]),
            {ok, State};
        {error, #pushy_message{validated=bad_timestamp}} ->
            pushy_logger:error("Bad timestamp in message: =~s", [Body]),
            {ok, State}
    catch
        error:Error ->
            Stack = erlang:get_stacktrace(),
            pushy_logger:error("Command message parser failed horribly: header=~p~nstack~p~s", [Error, Stack]),
            {ok, State};
        Error ->
            Stack = erlang:get_stacktrace(),
            pushy_logger:error("Command message parser failed horribly: header=~p~nstack~p~s", [Error, Stack]),
            {ok, State}
    end.

-spec process_message(State :: #state{},
                      Message :: #pushy_message{}) -> #state{}.
process_message(#state{node_ref=NodeRef, node_addr=CurAddr} = State, #pushy_message{address=NewAddr} = Message)
  when CurAddr =/= NewAddr ->
    %% Our address has changed. By this point we've validated the message, so we can trust the address
    pushy_logger:debug("Address change for ~p '~s' to '~s'~n",
               [NodeRef, pushy_tools:bin_to_hex(CurAddr),
                pushy_tools:bin_to_hex(NewAddr)]),
    GprocNewAddr = pushy_node_state_sup:mk_gproc_addr(NewAddr),
    gproc:reg({n, l, GprocNewAddr}),
    GprocCurAddr = pushy_node_state_sup:mk_gproc_addr(CurAddr),
    gproc:unreg({n, l, GprocCurAddr}),
    process_message(State#state{node_addr=NewAddr}, Message);
process_message(#state{node_ref=NodeRef, node_addr=Address} = State, #pushy_message{address=Address, body=Data}) ->
    JobId = extract_job_id(Data),
    BinaryType = ej:get({<<"type">>}, Data),
    IncarnationId = ej:get({<<"incarnation_id">>}, Data),
    Type = message_type_to_atom(BinaryType),
    pushy_logger:debug("Received message for Node ~p Type ~p (address ~p)",
                [NodeRef, BinaryType, pushy_tools:bin_to_hex(Address)]),
    case Type of
        unknown ->
            pushy_logger:error("Status message for node ~p was missing type field!~n", [NodeRef]),
            State;
        undefined ->
            pushy_logger:error("Status message for node ~p had unknown type ~p~n", [NodeRef, BinaryType]),
            State;
        heartbeat ->
            send_node_event(State, JobId, NodeRef, IncarnationId, heartbeat);
        _ ->
            send_node_event(State, JobId, NodeRef, Type)
    end.

-spec send_node_event(#state{}, any(), any(), any(), node_event()) -> #state{}.
send_node_event(State, JobId, NodeRef, IncarnationId, heartbeat) ->
    pushy_logger:debug("Received heartbeat for node ~p with job id ~p", [NodeRef, JobId]),
    case JobId /= null andalso pushy_job_state_sup:get_process(JobId) == not_found of
        true ->
            gen_fsm:send_event(self(), do_rehab);
        _ ->
            ok
    end,
    send_info(self(), {heartbeat, IncarnationId}),
    State.

-spec send_node_event(#state{}, any(), any(), node_event()) -> #state{}.
send_node_event(State, JobId, NodeRef, aborted = Msg) ->
    gen_fsm:send_event(self(), aborted),
    if
        JobId /= null ->
            pushy_job_state:send_node_event(JobId, NodeRef, Msg);
        true ->
            ok
    end,
    State;
send_node_event(State, JobId, NodeRef, Msg) ->
    case pushy_job_state:send_node_event(JobId, NodeRef, Msg) of
        not_found -> gen_fsm:send_event(self(), do_rehab);
        _ -> ok
    end,
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
do_send(#state{node_addr=NodeAddr, node_ref=NodeRef, sequence_no = SeqNo} = State,
        Method, Message) ->
    %% Normalize msg by adding standard fields.
    Message2 = pushy_messaging:insert_timestamp_and_sequence(Message, SeqNo),
    State2 = State#state{sequence_no = SeqNo+1},

    {ok, Key} = get_key_for_method(Method, NodeRef),
    Packets = ?TIME_IT(pushy_messaging, make_message, (proto_v2, Method, Key, Message2)),
    ok = pushy_command_switch:send([NodeAddr | Packets]),
    State2.

message_type_to_atom(<<"aborted">>) -> aborted;
message_type_to_atom(<<"ack_commit">>) -> ack_commit;
message_type_to_atom(<<"ack_run">>) -> ack_run;
message_type_to_atom(<<"failed">>) -> failed;
message_type_to_atom(<<"heartbeat">>) -> heartbeat;
message_type_to_atom(<<"nack_commit">>) -> nack_commit;
message_type_to_atom(<<"nack_run">>) -> nack_run;
message_type_to_atom(<<"succeeded">>) -> succeeded;
message_type_to_atom(undefined) -> undefined;
message_type_to_atom(_) -> unknown.

%%
%% TODO: This should be revisited when we tackle OC-5328 (handle ill formed packets set to pushy)
%%
extract_job_id(Data) ->
    case ej:get({<<"job_id">>}, Data) of
        <<"null">> ->
            null;
        null ->
            null;
        X when size(X) =< ?MAX_JOB_ID_LENGTH ->
            X;
        _ ->
            invalid_job_id
    end.
