%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([current_state/1,
         heartbeat/1,
         restarting/1,
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

-define(NO_NODE, {error, no_node}).
-define(NODE_EVENT(Event), Event(Name) -> case catch gproc:send({n,l,Name}, Event) of
                                              {'EXIT', _} -> ?NO_NODE;
                                              _ -> ok
                                          end).

%% gen_fsm callbacks
-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         terminate/3]).

-record(state, {dead_interval,
                name,
                heartbeats = 0,
                observers = [],
                tref}).

-include("pushy_sql.hrl").

start_link(Name, HeartbeatInterval, DeadIntervalCount) ->
    gen_fsm:start_link(?MODULE, [Name, HeartbeatInterval, DeadIntervalCount], []).

?NODE_EVENT(heartbeat).

?NODE_EVENT(restarting).


current_state(Name) ->
    case catch gproc:lookup_pid({n,l,Name}) of
        {'EXIT', _} ->
            ?NO_NODE;
        Pid ->
            gen_fsm:sync_send_event(Pid, current_state, infinity)
    end.

init([Name, HeartbeatInterval, DeadIntervalCount]) ->
    {ok, initializing, #state{dead_interval=HeartbeatInterval * DeadIntervalCount,
                              name=Name}, 0}.

initializing(timeout, #state{name=Name}=State) ->
    case gproc:reg({n, l, Name}) of
        true ->
            {next_state, down, reset_timer(save_status(down, State))};
        false ->
            error_logger:error_msg("Failed to register:~p for ~p~n", [Name,self()]),
            {stop, shutdown, State}
    end.

start_watching(Name) ->
    watching(start_watching, Name).

stop_watching(Name) ->
    watching(stop_watching, Name).


up(current_state, _From, State) ->
    {reply, up, up, State}.

crashed(current_state, _From, State) ->
    {reply, crashed, crashed, State}.

restarting(current_state, _From, State) ->
    {reply, restarting, restarting, State}.

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
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, StateName, State) ->
    Observers = State#state.observers,
    State1 = State#state{observers=lists:delete(Object,Observers)},
    {next_state, StateName, State1};

handle_info(heartbeat, up, State) ->
    {next_state, up, reset_timer(State)};
handle_info(heartbeat, crashed, State) ->
    confirm_heartbeat_threshold(State, crashed);
handle_info(heartbeat, down, State) ->
    confirm_heartbeat_threshold(State, down);
handle_info(heartbeat, restarting, State) ->
    {next_state, up, reset_timer(save_status(up, State))};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions

watching(Action, Name) ->
    case catch gproc:lookup_pid({n,l,Name}) of
        {'EXIT', _} ->
            ?NO_NODE;
        Pid ->
            gen_fsm:send_all_state_event(Pid, {Action, self()})
    end.

save_status(Status, #state{name=Name}=State) ->
    notify_status_change(Status, State),
    NodeStatus = pushy_object:new_record(pushy_node_status,
                                        ?POC_ORG_ID,
                                        [{<<"node">>, Name},{<<"type">>, Status}]),
    case pushy_object:create_object(create_node_status, NodeStatus, ?POC_ACTOR_ID) of
        {ok, _} ->
            State;
        {conflict, _} ->
            pushy_object:update_object(update_node_status, NodeStatus, ?POC_ACTOR_ID),
            State;
        {error, _Error} ->
            State
    end.

notify_status_change(Status, #state{name=Name,observers=Observers}) ->
    [ Observer ! { Name,self(),Status } || Observer <- Observers ].


%% confirm that we have recieved enough heartbeats before coming up
confirm_heartbeat_threshold(#state{heartbeats=HeartBeats}=State, StateName) ->
    if HeartBeats >= ?POC_HB_THRESHOLD  ->
            {next_state, up, reset_timer(save_status(up, State))};
       true ->
            State1 = State#state{heartbeats=HeartBeats+1},
            {next_state, StateName, reset_timer(State1)}
    end.

reset_timer(#state{dead_interval=Interval, tref=undefined}=State) ->
    TRef = erlang:send_after(Interval, self(), no_heartbeats),
    State#state{tref=TRef};
reset_timer(#state{dead_interval=Interval, tref=TRef}=State) ->
    erlang:cancel_timer(TRef),
    TRef1 = erlang:send_after(Interval, self(), down),
    State#state{tref=TRef1}.
