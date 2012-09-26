%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Mark Anderson <mark@opscode.com>
%% @author John Keiser <john@opscode.com>
%%
%% @copyright 2012 Opscode Inc.
%% @end

%%
%% @doc simple FSM for tracking node heartbeats and thus up/down status
%%
-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([current_state/1,
         heartbeat/1,
         set_logging/2,
         start_link/1,
         start_link/2]).

%% Observers
-export([start_watching/1,
         stop_watching/1]).

-define(SAVE_MODE, gen_server). % direct or gen_server
-define(NO_NODE, {error, no_node}).

%% gen_fsm callbacks
-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         init/2,
         terminate/3]).

-include("pushy.hrl").
-include("pushy_sql.hrl").

-include_lib("eunit/include/eunit.hrl").

-type logging_level() :: 'verbose' | 'normal'.

-type eavg() :: any().

-define(DEFAULT_DECAY_INTERVAL, 4).
-define(DEFAULT_UP_THRESHOLD, 0.5).
-define(DEFAULT_DOWN_THRESHOLD, 0.4).

-record(state, {node_ref              :: node_ref(),
                heartbeat_interval    :: integer(),
                decay_window          :: integer(),
                logging = normal      :: logging_level(),
                current_status = down :: node_status(),
                heartbeats_rcvd = 0   :: integer(),
                up_threshold          :: float(),
                down_threshold        :: float(),
                tref,
                heartbeat_rate   :: eavg()
               }).

%%%
%%% External API
%%%
-spec start_link(node_ref() ) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(NodeRef) ->
    start_link(NodeRef, down).

-spec start_link(node_ref(), 'up' | 'down' ) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(NodeRef, StartState) ->
    gen_fsm:start_link(?MODULE, {NodeRef, StartState}, []).

-spec heartbeat(node_ref()) -> 'ok'.
heartbeat(NodeRef) ->
    Pid = pushy_node_state_sup:get_process(NodeRef),
    gen_fsm:send_all_state_event(Pid, heartbeat).

-spec current_state(node_ref()) -> node_status().
current_state(NodeRef) ->
    Pid = pushy_node_state_sup:get_process(NodeRef),
    gen_fsm:sync_send_all_state_event(Pid, current_state, infinity).

-spec set_logging(node_ref(), logging_level()) -> ok.
set_logging(NodeRef, Level) when Level =:= verbose orelse Level =:= normal ->
    Pid = pushy_node_state_sup:get_process(NodeRef),
    gen_fsm:send_all_state_event(Pid, {logging, Level}).

-spec start_watching(node_ref()) -> true.
start_watching(NodeRef) ->
    gproc:reg(subscribers_key(NodeRef)).

-spec stop_watching(node_ref()) -> true.
stop_watching(NodeRef) ->
    try
        gproc:unreg(subscribers_key(NodeRef))
    catch error:badarg ->
            ok
    end.

init({NodeRef,StartState}) ->
    init(NodeRef,StartState);
init(NodeRef) ->
    init(NodeRef, down).
%
% This is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
init(NodeRef, StartState) ->
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, GprocName}),
        HeartbeatInterval = pushy_util:get_env(pushy, heartbeat_interval, fun is_integer/1),
        DecayWindow = pushy_util:get_env(pushy, decay_window, ?DEFAULT_DECAY_INTERVAL, fun is_integer/1),
        UpThresh   = pushy_util:get_env(pushy, up_threshold, ?DEFAULT_UP_THRESHOLD, any), %% TODO constrain to float
        DownThresh = pushy_util:get_env(pushy, down_threshold, ?DEFAULT_DOWN_THRESHOLD, any), %% TODO constrain to float

        InitAvg = case StartState of
                      up -> 1.0;
                      down -> 0.0
                  end,

        State = #state{node_ref = NodeRef,
                       decay_window = DecayWindow,
                       heartbeat_interval = HeartbeatInterval,
                       heartbeat_rate = pushy_ema:init(DecayWindow, HeartbeatInterval, InitAvg),
                       up_threshold = UpThresh,
                       down_threshold = DownThresh,
                       current_status = StartState
                      },
        {ok, StartState, create_status_record(StartState, State)}
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

%%
%% These events are handled the same for every state
%%
-spec handle_event(any(), node_status(), #state{}) -> {any(), node_status(), #state{}}.
handle_event({logging, Level}, StateName, State) ->
    State1 = State#state{logging=Level},
    {next_state, StateName, State1};
handle_event(heartbeat,
            StateName,
            #state{node_ref=NodeRef, heartbeats_rcvd=HeartBeats, logging=Level, current_status=CurStatus, heartbeat_rate=HRate}=State) ->
    nlog(Level, "Heartbeat received from ~p. Currently ~p / ~p", [NodeRef, CurStatus, HRate]),
    %% Note that we got a heartbeat
    State1 = State#state{
        heartbeat_rate=pushy_ema:inc(HRate,1),
        current_status=CurStatus,
        heartbeats_rcvd=HeartBeats+1
    },
    {next_state, StateName, State1};
handle_event(Event, StateName, #state{node_ref=NodeRef}=State) ->
    lager:error("FSM for ~p received unexpected handle_event(~p)", [NodeRef, Event]),
    {next_state, StateName, State}.

handle_sync_event(current_state, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(Event, _From, StateName, #state{node_ref=NodeRef}=State) ->
    lager:error("FSM for ~p received unexpected handle_sync_event(~p)", [NodeRef, Event]),
    {reply, ignored, StateName, State}.

%%
%% Handle info
%%
handle_info(down, down, State) ->
    {next_state, down, State};
handle_info({timeout, _Ref, update_avg}, CurStatus, #state{heartbeat_rate=HRate, up_threshold=UThresh, down_threshold=DThresh}=State) ->
    NHRate = pushy_ema:tick(HRate),
    EAvg = pushy_ema:value(NHRate),
    {NStatus, NState} =
        case CurStatus of
            up when EAvg < DThresh ->
                NS = update_status(down, State),
                {down, NS};
            down when EAvg > UThresh ->
                NS = update_status(up, State),
                {up, NS};
            S -> {S, State}
        end,
    {next_state, NStatus, NState#state{heartbeat_rate=NHRate} };
handle_info(Info, CurState, #state{node_ref=NodeRef}=State) ->
    lager:error("FSM for ~p received unexpected handle_info(~p)", [NodeRef, Info]),
    {next_state, CurState, State}.


terminate(_Reason, _CurState, _State) ->
    ok.

code_change(_OldVsn, CurState, State, _Extra) ->
    {ok, CurState, State}.

%% Internal functions

create_status_record(Status, #state{node_ref=NodeRef}=State) ->
    notify_status_change(Status, State),
    pushy_node_status_updater:create(NodeRef, ?POC_ACTOR_ID, Status),
    State.

update_status(Status, #state{node_ref=NodeRef}=State) ->
    notify_status_change(Status, State),
    pushy_node_status_updater:update(NodeRef, ?POC_ACTOR_ID, Status),
    State.

notify_status_change(Status, #state{node_ref=NodeRef}) ->
    lager:info("Status change for ~p : ~p", [NodeRef, Status]),
    case Status of
        down -> gproc:send(subscribers_key(NodeRef), {down, NodeRef});
        up -> nop
    end.

nlog(normal, Format, Args) ->
    lager:debug(Format, Args);
nlog(verbose, Format, Args) ->
    lager:info(Format, Args).

-spec subscribers_key(node_ref()) -> {p,l,{node_state_monitor,node_ref()}}.
subscribers_key(NodeRef) ->
    {p,l,{node_state_monitor,NodeRef}}.
