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

start_link(NodeRef, NodeAddr) ->
    gen_fsm:start_link(?MODULE, [NodeRef, NodeAddr], []).

heartbeat(NodeRef) ->
    send_info(NodeRef, heartbeat),
    ok.

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
    case pushy_node_state_sup:get_or_create_process(NodeRef) of
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
    pushy_node_status_updater:update(NodeRef, ?POC_ACTOR_ID, New),
    notify_watchers(Watchers, NodeRef, Current, New),
    New.

notify_watchers([], _NodeRef, _Current, _New) ->
    ok;
notify_watchers(Watchers, NodeRef, Current, New) ->
    F = fun(Watcher) -> Watcher ! {state_change, NodeRef, Current, New} end,
    [F(Watcher) || {Watcher, _Monitor} <- Watchers].
