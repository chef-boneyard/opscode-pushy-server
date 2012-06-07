%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([current_state/1,
         heartbeat/1,
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

heartbeat({heartbeat, NodeName, NodeState}) ->
    case catch gproc:send({n,l,NodeName},
            {heartbeat, NodeName, status_to_atom(NodeState)}) of
        {'EXIT', _} -> ?NO_NODE;
        _ -> ok
    end.


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
handle_info({heartbeat, NodeName, NodeState}, StateName, #state{heartbeats=HeartBeats}=State) ->
    error_logger:info_msg("Heartbeat recieved from ~p Currently ~p~n", [NodeName, NodeState]),
    if HeartBeats >= ?POC_HB_THRESHOLD  ->
            {next_state, NodeState, reset_timer(save_status(NodeState, State))};
       true ->
           {next_state, StateName, State#state{heartbeats=HeartBeats+1}}
    end;
handle_info(down, _StateName, State) ->
    {next_state, down, save_status(down, State#state{heartbeats=0})};
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

status_to_atom(<<"idle">>) ->
    idle;
status_to_atom(<<"ready">>) ->
    ready;
status_to_atom(<<"running">>) ->
    running;
status_to_atom(<<"restarting">>) ->
    restarting.

reset_timer(#state{dead_interval=Interval, tref=undefined}=State) ->
    TRef = erlang:send_after(Interval, self(), no_heartbeats),
    State#state{tref=TRef};
reset_timer(#state{dead_interval=Interval, tref=TRef}=State) ->
    erlang:cancel_timer(TRef),
    TRef1 = erlang:send_after(Interval, self(), down),
    State#state{tref=TRef1}.
