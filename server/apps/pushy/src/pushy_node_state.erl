%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([current_state/1,
         heartbeat/1,
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
-type logging_level() :: 'verbose' | 'normal'.
-type status_atom() :: 'idle' | 'ready' | 'restarting' | 'running'.
-type status_binary() :: binary(). % <<"idle">> | <<"ready">> | <<"restarting">> | <<"running">>.
-type gproc_error() :: ok | {error, no_node}.

-record(eavg, {acc = 0 :: integer(),
               avg = 0 :: float(),
               n = 0   :: integer(),
               tick_interval :: integer(),
               tref   :: reference()
              }).

-record(state, {dead_interval    :: integer(),
                name             :: binary(),
                heartbeats = 0   :: integer(),
                logging = normal :: logging_level(),
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
    ?debugVal(EAvg),
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

-spec heartbeat({'heartbeat',node_name(),node_state()}) -> gproc_error().
heartbeat({heartbeat, NodeName, NodeState}) ->
    case catch gproc:send({n,l,NodeName},
            {heartbeat, NodeName, status_to_atom(NodeState)}) of
        {'EXIT', _} -> ?NO_NODE;
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


init([Name, HeartbeatInterval, DeadIntervalCount]) ->
    {ok, initializing,
     #state{dead_interval=HeartbeatInterval * DeadIntervalCount,
            name=Name,
            heartbeat_rate=eavg_init(DeadIntervalCount, HeartbeatInterval)
           },
     0}.

initializing(timeout, #state{name=Name}=State) ->
    case gproc:reg({n, l, Name}) of
        true ->
            {next_state, down, reset_timer(save_status(?SAVE_MODE, down, State))};
        false ->
            lager:error("Failed to register:~p for ~p", [Name,self()]),
            {stop, shutdown, State}
    end.


%%%
%%% State machine internals
%%%
up(current_state, _From, State) ->
    {reply, up, up, State}.

crashed(current_state, _From, State) ->
    {reply, crashed, crashed, State}.

restarting(current_state, _From, State) ->
    {reply, restarting, restarting, State}.

%%
%% These events are handled the same everywhere
%%
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
handle_event(_Event, StateName, #state{name=Name}=State) ->
    lager:error("FSM for ~p received unexpected event ~p", [Name, _Event]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.


handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, StateName, State) ->
    Observers = State#state.observers,
    State1 = State#state{observers=lists:delete(Object,Observers)},
    {next_state, StateName, State1};
handle_info({heartbeat, NodeName, NodeState},
            StateName, #state{logging=Level, heartbeats=HeartBeats, heartbeat_rate=HRate}=State) ->
    nlog(Level, "Heartbeat recieved from ~p Currently ~p ~p", [NodeName, NodeState, HRate]),
    eavg_value(HRate),
    %% Note that we got a heartbeat
    State1 = State#state{heartbeat_rate=eavg_inc(HRate,1)},

    case HeartBeats of
        ?POC_HB_THRESHOLD - 1 ->
            State2 = reset_timer(save_status(?SAVE_MODE, NodeState, State1)),
            {next_state, NodeState, State2#state{heartbeats=HeartBeats + 1}};
        ?POC_HB_THRESHOLD ->
            {next_state, StateName, State1};
        _ ->
            {next_state, NodeState, reset_timer(save_status(?SAVE_MODE, NodeState, State1))}
    end;
handle_info(down, _StateName, State) ->
    {next_state, down, save_status(?SAVE_MODE, down, State#state{heartbeats=0})};
handle_info({timeout, _Ref, update_avg}, StateName, #state{heartbeat_rate=HRate}=State) ->
    {next_state, StateName, State#state{heartbeat_rate=eavg_tick(HRate)} };
handle_info(Info, StateName, State) ->
    lager:error("Strange message ~p received in state ~p", [Info, StateName]),
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions

save_status(direct, Status, State) ->
    notify_status_change(Status, State),
    save_status_directly(Status, State);
save_status(gen_server, Status, #state{name=Name}=State) ->
    notify_status_change(Status, State),
    pushy_node_status_updater:update(?POC_ORG_ID, Name, ?POC_ACTOR_ID, Status),
    State.

save_status_directly(Status, #state{name=Name}=State) ->
    NodeStatus = pushy_object:new_record(pushy_node_status,
                                         ?POC_ORG_ID,
                                         [{<<"node">>, Name},{<<"type">>, Status}]),
    %% This probably should be refactored into a 'init_status' and a 'save_status'; once the
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
    restarting.

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


