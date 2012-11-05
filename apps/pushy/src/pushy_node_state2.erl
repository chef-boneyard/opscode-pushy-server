-module(pushy_node_state2).

-behaviour(gen_fsm).

-include("pushy.hrl").

-type logging_level() :: 'verbose' | 'normal'.
-type eavg() :: any().


%% API
-export([start_link/1,
         status/1]).

%% States
-export([booting/2,
         idle/2,
         running/2,
         rehab/2]).

-define(DEFAULT_DECAY_INTERVAL, 4).
-define(DEFAULT_UP_THRESHOLD, 0.5).
-define(DEFAULT_DOWN_THRESHOLD, 0.4).

-record(state, {node_ref              :: node_ref(),
                heartbeat_interval    :: integer(),
                decay_window          :: integer(),
                logging = verbose     :: logging_level(),
                heartbeats_rcvd = 0   :: integer(),
                up_threshold          :: float(),
                down_threshold        :: float(),
                job                   :: any(),
                rehab_timer,
                heartbeat_rate   :: eavg()
               }).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

start_link(NodeRef) ->
    gen_fsm:start_link(?MODULE, [NodeRef], []).

status(NodeRef) ->
    case pushy_node_state_sup:get_process(NodeRef) of
        undefined ->
            {offline, unavailable};
        Pid ->
            eval_state_name(gen_fsm:sync_send_all_state_event(Pid, current_state, infinity))
    end.

init([NodeRef]) ->
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, GprocName}),
        HeartbeatInterval = heartbeat_interval(),
        DecayWindow = envy:get(pushy, decay_window, ?DEFAULT_DECAY_INTERVAL, integer),
        UpThresh   = envy:get(pushy, up_threshold, ?DEFAULT_UP_THRESHOLD, number), %% TODO constrain to float
        DownThresh = envy:get(pushy, down_threshold, ?DEFAULT_DOWN_THRESHOLD, number), %% TODO constrain to float
        {ok, TimerRef} = timer:send_interval(rehab_interval(), send_abort),

        State = #state{node_ref = NodeRef,
                       decay_window = DecayWindow,
                       heartbeat_interval = HeartbeatInterval,
                       heartbeat_rate = pushy_ema:init(DecayWindow, HeartbeatInterval, 1.0),
                       up_threshold = UpThresh,
                       down_threshold = DownThresh,
                       rehab_timer=TimerRef},
        {ok, booting, State}
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
            {stop, shutdown, undefined}
    end.

rehab(aborted, #state{rehab_timer=TRef}=State) ->
    timer:cancel(TRef),
    {next_state, idle, State};
rehab(Message, #state{node_ref=NodeRef}=State) ->
    error_logger:error_msg("~p in rehab. Ignoring message: ~p~n", [NodeRef, Message]),
    Message = {[{type, abort}]},
    ok = pushy_command_switch:send_command(NodeRef, Message),
    {next_state, rehab, State}.

idle({job, Job}, State) ->
    {next_state, running, State#state{job=Job}}.

running(aborted, State) ->
    {next_state, idle, State};
running({complete, Job}, #state{job=Job}=State) ->
    {next_state, idle, State}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(current_state, _From, StateName, State) ->
    {reply, StateName, StateName, State}.

handle_info(heartbeat, booting, #state{node_ref=NodeRef, heartbeats_rcvd=HeartBeats, heartbeat_rate=HRate, up_threshold=UThresh}=State) ->
    NHRate = pushy_ema:tick(HRate),
    EAvg = pushy_ema:value(NHRate),
    State1 = State#state{heartbeat_rate=NHRate, heartbeats_rcvd=HeartBeats + 1},
    case EAvg > UThresh of
        true ->
            {next_state, idle, State1, heartbeat_interval()};
        false ->
            Message = {[{type, abort}]},
            ok = pushy_command_switch:send_command(NodeRef, Message),
            {next_state, rehab, State1, heartbeat_interval()}
    end;
handle_info(send_abort, rehab, #state{node_ref=NodeRef}=State) ->
    Message = {[{type, abort}]},
    ok = pushy_command_switch:send_command(NodeRef, Message),
    {next_state, rehab, State};
handle_info(timeout, CurrentState, #state{heartbeat_rate=HRate, down_threshold=DThresh}=State) ->
    NHRate = pushy_ema:tick(HRate),
    EAvg = pushy_ema:value(NHRate),
    NState = State#state{heartbeat_rate=NHRate},
    case EAvg < DThresh of
        true ->
            {stop, shutdown, NState};
        false ->
            {next_state, CurrentState, NState}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
eval_state_name(booting) ->
    {online, unvailable};
eval_state_name(idle) ->
    {online, available};
eval_state_name(rehab) ->
    {online, unavailable};
eval_state_name(running) ->
    {online, unvailable}.

rehab_interval() ->
    envy:get(pushy, rehab_timer, 1000, integer).

heartbeat_interval() ->
    envy:get(pushy, heartbeat_interval, integer).
