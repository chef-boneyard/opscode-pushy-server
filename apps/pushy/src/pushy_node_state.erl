-module(pushy_node_state).

-behaviour(gen_fsm).

-include("pushy.hrl").

%% API
-export([start_link/1,
         heartbeat/1,
         status/1,
         watch/1,
         aborted/1]).

%% States
-export([booting/2,
         idle/2,
         running/2,
         rehab/2]).

-define(DEFAULT_DECAY_INTERVAL, 4).
-define(DEFAULT_UP_THRESHOLD, 0.5).
-define(DEFAULT_DOWN_THRESHOLD, 0.4).

-record(state, {node_ref              :: node_ref(),
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

start_link(NodeRef) ->
    gen_fsm:start_link(?MODULE, [NodeRef], []).

heartbeat(NodeRef) ->
    send_info(NodeRef, heartbeat),
    ok.

status(NodeRef) ->
    case call(NodeRef, current_state) of
        undefined ->
            {offline, unavailable};
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
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            gen_fsm:send_event(Pid, aborted);
        Error ->
            Error
    end.


init([NodeRef]) ->
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    State = #state{node_ref = NodeRef},
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, GprocName}),
        State = #state{node_ref = NodeRef},
        {ok, state_transition(init, booting, State), State}
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

booting(Message, #state{node_ref=NodeRef}=State) ->
    lager:info("~p is booting. Ignoring message: ~p~n", [NodeRef, Message]),
    {next_state, booting, State, 60000}.

rehab(aborted, #state{state_timer=TRef}=State) ->
    timer:cancel(TRef),
    {next_state, state_transition(rehab, idle, State), State};
rehab(Message, #state{node_ref=NodeRef}=State) ->
    lager:info("~p in rehab. Ignoring message: ~p~n", [NodeRef, Message]),
    {next_state, rehab, State}.

idle({job, Job}, State) ->
    {next_state, state_transition(idle, running, State), State#state{job=Job}}.

running(aborted, #state{node_ref=NodeRef}=State) ->
    lager:info("~p aborted during job.~n", [NodeRef]),
    {next_state, state_transition(running, idle, State), State};
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

handle_info(heartbeat, booting, #state{heartbeats=HBeats}=State) ->
    HBeats1 = HBeats + 1,
    State1 = State#state{heartbeats=HBeats1},
    case HBeats1 > 3 of
        true ->
            State2 = force_abort(State1),
            {next_state, state_transition(booting, rehab, State2), State2};
        false ->
            {next_state, booting, State1}
    end;
handle_info(heartbeat, CurrentState, #state{heartbeats=HBeats}=State) ->
    HBeats1 = HBeats + 1,
    State1 = if
                 HBeats1 < 5 ->
                     State#state{heartbeats=HBeats1};
                 true ->
                     State#state{heartbeats=5}
             end,
    {next_state, CurrentState, State1};
handle_info(timeout, CurrentState, #state{heartbeats=HBeats}=State) ->
    HBeats1 = HBeats - 1,
    State1 = State#state{heartbeats=HBeats1},
    case HBeats1 of
        0 ->
            {stop, state_transition(CurrentState, shutdown, State1), State1};
        _ ->
            {next_state, CurrentState, State, heartbeat_interval()}
    end;
handle_info(rehab_again, rehab, State) ->
    State1 = force_abort(State),
    {next_state, rehab, State1};
handle_info({'DOWN', _MRef, _Type, Pid, _Reason}, StateName, #state{watchers=Watchers}=State) ->
    case lists:keytake(Pid, Watchers) of
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
eval_state({booting, undefined}) ->
    {online, {unvailable, none}};
eval_state({idle, undefined}) ->
    {online, {available, none}};
eval_state({rehab, undefined}) ->
    {online, {unavailable, none}};
eval_state({running, Job}) ->
    {online, {unvailable, Job}}.

rehab_interval() ->
    envy:get(pushy, rehab_timer, 1000, integer).

heartbeat_interval() ->
    envy:get(pushy, heartbeat_interval, integer).

call(NodeRef, Message) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        Pid when is_pid(Pid) ->
            gen_fsm:sync_send_all_state_event(Pid, Message, infinity);
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

force_abort(#state{node_ref=NodeRef}=State) ->
    Message = {[{type, abort}]},
    ok = pushy_command_switch:send_command(NodeRef, Message),
    TRef = timer:send_after(rehab_interval(), rehab_again),
    State#state{state_timer=TRef}.

state_transition(Current, New, #state{node_ref=NodeRef, watchers=Watchers}) ->
    lager:debug("~p transitioning from ~p to ~p~n", [NodeRef, Current, New]),
    notify_watchers(Watchers, NodeRef, Current, New),
    New.

notify_watchers([], _NodeRef, _Current, _New) ->
    ok;
notify_watchers(Watchers, NodeRef, Current, New) ->
    F = fun(Watcher) -> Watcher ! {state_change, NodeRef, Current, New} end,
    [F(Watcher) || {Watcher, _Monitor} <- Watchers].
