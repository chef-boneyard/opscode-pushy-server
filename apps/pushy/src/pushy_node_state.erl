%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([current_state/1,
         heartbeat/2,
         set_logging/2,
         start_link/3]).

%% Observers
-export([start_watching/1,
         stop_watching/1]).

%% States
-export([initializing/2]).

%% Event handlers
-export([crashed/3,
         restarting/3,
         up/3]).

-define(SAVE_MODE, gen_server). % direct or gen_server
-define(NO_NODE, {error, no_node}).

%% gen_fsm callbacks
-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         terminate/3]).

-include("pushy_sql.hrl").

-include_lib("eunit/include/eunit.hrl").

-type node_name() :: binary().
-type node_state() :: binary().
-type fsm_states() :: 'up' | 'down' | 'crashed' | 'restarting'.
-type logging_level() :: 'verbose' | 'normal'.
-type status_atom() :: 'idle' | 'ready' | 'restarting' | 'running' | 'down'.
-type status_binary() :: binary(). % <<"idle">> | <<"ready">> | <<"restarting">> |
                                   % <<"running">> | <<"down">>
-type gproc_error() :: ok | {error, no_node}.

-record(eavg, {acc = 0 :: integer(),
               avg = 0 :: float(),
               n = 0   :: integer(),
               tick_interval :: integer(),
               tref   :: reference()
              }).

-record(state, {name                  :: binary(),
                dead_interval         :: integer(),
                heartbeats = 0        :: integer(),
                logging = normal      :: logging_level(),
                current_status = down :: status_atom(),
                observers = [],
                tref,
                heartbeat_rate   :: #eavg{}
               }).

-spec eavg_init(integer(), integer()) -> #eavg{}.
eavg_init(N, I) ->
    TRef = erlang:start_timer(I, self(), update_avg),
    #eavg{acc=0, avg=0, n=N, tick_interval=I, tref=TRef}.

-spec eavg_tick(#eavg{}) -> #eavg{}.
eavg_tick(#eavg{acc=Acc, avg=Avg, n=N, tick_interval=I}=EAvg) ->
    NAvg = (Avg * (N-1) + Acc)/N,
    TRef = erlang:start_timer(I, self(), update_avg),
    EAvg#eavg{acc=0, avg=NAvg, tref=TRef}.

eavg_inc(#eavg{acc=Acc}=EAvg, Count) ->
    EAvg#eavg{acc=Acc+Count}.

eavg_value(#eavg{avg=Avg}) ->
    Avg.

%%%
%%% External API
%%%
-spec start_link(node_name(),HeartbeatInterval::integer(),DeadIntervalCount::integer()) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link(Name, HeartbeatInterval, DeadIntervalCount) ->
    gen_fsm:start_link(?MODULE, [Name, HeartbeatInterval, DeadIntervalCount], []).

-spec heartbeat(node_name(), node_state()) -> 'ok'.
heartbeat(NodeName, NodeStatus) ->
    case catch gproc:send({n,l,NodeName},
            {heartbeat, NodeName, status_to_atom(NodeStatus)}) of
        {'EXIT', _} ->
            % TODO this fails to take into account a failed initialize/gproc registration
            pushy_node_state_sup:new(NodeName),
            heartbeat(NodeName, NodeStatus);
        _ -> ok
    end.

-spec current_state(node_name()) -> any().
current_state(Name) ->
    case catch gproc:lookup_pid({n,l,Name}) of
        {'EXIT', _} ->
            ?NO_NODE;
        Pid ->
            gen_fsm:sync_send_event(Pid, current_state, infinity)
    end.


-spec set_logging(node_name(), logging_level()) ->  gproc_error().
set_logging(Name, verbose) ->
    set_logging(Name, verbose, ok);
set_logging(Name, normal) ->
    set_logging(Name, normal, ok).

set_logging(Name, Level, ok) ->
    case catch gproc:lookup_pid({n,l,Name}) of
        {'EXIT', _} ->
            ?NO_NODE;
        Pid ->
            gen_fsm:send_all_state_event(Pid, {logging, Level})
    end.

-spec start_watching(node_name()) -> gproc_error().
start_watching(Name) ->
    watching(start_watching, Name).

-spec stop_watching(node_name()) -> gproc_error().
stop_watching(Name) ->
    watching(stop_watching, Name).

-spec watching('start_watching' | 'stop_watching', node_name()) -> gproc_error().
watching(Action, Name) ->
    case catch gproc:lookup_pid({n,l,Name}) of
        {'EXIT', _} ->
            ?NO_NODE;
        Pid ->
            gen_fsm:send_all_state_event(Pid, {Action, self()})
    end.

%
% This is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
% 
init([Name, HeartbeatInterval, DeadIntervalCount]) ->

    State = #state{name = Name,
                   dead_interval = HeartbeatInterval * DeadIntervalCount,
                   heartbeats = 0,
                   current_status = down, % Fetch from db later
                   heartbeat_rate = eavg_init(DeadIntervalCount, HeartbeatInterval)},
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, Name}),
        {ok, initializing, State, 0}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            lager:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [Name,self(), gproc:lookup_pid({n,l,Name}) ]),
            {stop, shutdown, State}
    end.

%
% Lower half of initialization; we have more time for complex work here.
%
initializing(timeout, #state{}=StateData) ->
    StateData2 = StateData#state{
                   current_status = down % TODO Fetch this from the db someday
                   },
    {next_state, down, reset_timer(create_status_record(?SAVE_MODE, down, StateData2))}.

%%%
%%% State machine internals
%%%
up(current_state, _From, #state{heartbeat_rate=HRate}=State) ->
    {reply, {up, eavg_value(HRate)}, up, State}.

crashed(current_state, _From, #state{heartbeat_rate=HRate}=State) ->
    {reply, {crashed, eavg_value(HRate)}, crashed, State}.

restarting(current_state, _From, #state{heartbeat_rate=HRate}=State) ->
    {reply, {restarting, eavg_value(HRate)}, restarting, State}.

%%
%% These events are handled the same for every state
%%
-spec handle_event(any(), fsm_states(), #state{}) -> {any(), fsm_states(), #state{}}.
handle_event({start_watching, Who}, StateName, #state{observers=Observers}=State) ->
    State1 = case lists:member(Who, Observers) of
        false ->
            erlang:monitor(process, Who),
            State#state{observers=[Who|Observers]};
        true -> State
    end,
    {next_state, StateName, State1};
handle_event({stop_watching, Who}, StateName, #state{observers=Observers}=State) ->
    State1 = State#state{observers=lists:delete(Who, Observers)},
    {next_state, StateName, State1};
handle_event({logging, Level}, StateName, State) ->
    State1 = State#state{logging=Level},
    {next_state, StateName, State1};
handle_event(Event, StateName, #state{name=Name}=State) ->
    lager:error("FSM for ~p received unexpected event ~p", [Name, Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, #state{name=Name}=State) ->
    lager:error("FSM for ~p received unexpected sync event ~p", [Name, Event]),
    {reply, ignored, StateName, State}.

%%
%% Handle info
%%
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, CurState, State) ->
    Observers = State#state.observers,
    State1 = State#state{observers=lists:delete(Object,Observers)},
    {next_state, CurState, State1};
handle_info({heartbeat, NodeName, NodeStatus},
            CurState,
            #state{heartbeats=HeartBeats, logging=Level, current_status=CurStatus, heartbeat_rate=HRate}=State) ->
    nlog(Level, "Heartbeat recieved from ~p Currently ~p ~p", [NodeName, NodeStatus, HRate]),

    %% Note that we got a heartbeat
    State1 = State#state{heartbeat_rate=eavg_inc(HRate,1), current_status=NodeStatus},
    %% Reset the timer
    State2 = reset_timer(State1),

    case HeartBeats of
        ?POC_HB_THRESHOLD - 1 -> % transitioning up
            update_status(?SAVE_MODE, NodeStatus, State2),
            {next_state, up, State2#state{heartbeats=HeartBeats + 1}};
        ?POC_HB_THRESHOLD -> % up, only do something if status changes
            case NodeStatus of
                CurStatus -> ok;
                _Else -> update_status(?SAVE_MODE, NodeStatus, State2)
            end,
            {next_state, up, State2};
        _ -> % we're down, don't save state changes
            {next_state, CurState, State2#state{heartbeats=HeartBeats +1}}
    end;
handle_info(down, down, State) ->
    {next_state, down, State};
handle_info(down, _CurState, State) ->
    {next_state, down, update_status(?SAVE_MODE, down, State#state{heartbeats=0})};
handle_info({timeout, _Ref, update_avg}, CurState, #state{heartbeat_rate=HRate}=State) ->
    {next_state, CurState, State#state{heartbeat_rate=eavg_tick(HRate)} };
handle_info(no_heartbeats, _CurState, State) ->
    State1 = State#state{heartbeats=0, tref=undefined},
    update_status(?SAVE_MODE, down, State1),
    {next_state, down, State1};
handle_info(Info, CurState, State) ->
    lager:error("Strange message ~p received in state ~p", [Info, CurState]),
    {next_state, CurState, State}.


terminate(_Reason, _CurState, _State) ->
    ok.

code_change(_OldVsn, CurState, State, _Extra) ->
    {ok, CurState, State}.

%% Internal functions

create_status_record(direct, Status, State) ->
    notify_status_change(Status, State),
    update_status_directly(Status, State);
create_status_record(gen_server, Status, #state{name=Name}=State) ->
    notify_status_change(Status, State),
    pushy_node_status_updater:create(?POC_ORG_ID, Name, ?POC_ACTOR_ID, Status),
    State.

update_status(direct, Status, State) ->
    notify_status_change(Status, State),
    update_status_directly(Status, State);
update_status(gen_server, Status, #state{name=Name}=State) ->
    notify_status_change(Status, State),
    pushy_node_status_updater:update(?POC_ORG_ID, Name, ?POC_ACTOR_ID, Status),
    State.

update_status_directly(Status, #state{name=Name}=State) ->
    NodeStatus = pushy_object:new_record(pushy_node_status,
                                         ?POC_ORG_ID,
                                         [{<<"node">>, Name},{<<"type">>, Status}]),
    %% This probably should be refactored into a 'init_status' and a 'update_status'; once the
    %% FSM is started we should be able to assume the object exists, and go straight for update..
    case pushy_object:create_object(create_node_status, NodeStatus, ?POC_ACTOR_ID) of
        {ok, _} ->
            State;
        {conflict, _} ->
            pushy_object:update_object(update_node_status, NodeStatus, ?POC_ACTOR_ID),
            State;
        {error, _Error} ->
            %% Droping the status on the floor is a bad thing. Probably should mark this as
            %% needing retry somehow...
            State
    end.

notify_status_change(Status, #state{name=Name,observers=Observers}) ->
    lager:info("Status change for ~s : ~s", [Name, Status]),
    [ Observer ! { node_heartbeat_event, Name, Status } || Observer <- Observers ].

-spec status_to_atom(status_binary()) -> status_atom().
status_to_atom(<<"idle">>) ->
    idle;
status_to_atom(<<"ready">>) ->
    ready;
status_to_atom(<<"running">>) ->
    running;
status_to_atom(<<"restarting">>) ->
    restarting;
status_to_atom(<<"down">>) ->
    down.

%
% Question: Why isn't the gen_fsm:start_timer/cancel_timer used here?
%
reset_timer(#state{dead_interval=Interval, tref=undefined}=State) ->
    TRef = erlang:send_after(Interval, self(), no_heartbeats),
    State#state{tref=TRef};
reset_timer(#state{dead_interval=Interval, tref=TRef}=State) ->
    erlang:cancel_timer(TRef),
    TRef1 = erlang:send_after(Interval, self(), down),
    State#state{tref=TRef1}.

nlog(normal, Format, Args) ->
    lager:debug(Format, Args);
nlog(verbose, Format, Args) ->
    lager:info(Format, Args).


