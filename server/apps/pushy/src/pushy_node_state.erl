%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_state).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
         current_state/1,
         heartbeat/1,
         node_restarting/1]).

%% States
-export([initializing/2]).

%% Event handlers
-export([up/3,
         crashed/3,
         restarting/3]).

-define(NO_NODE, {error, no_node}).
-define(NODE_EVENT(Event), Event(Name) -> case catch gproc:send({n,l,Name}, Event) of
                                              {'EXIT', _} -> ?NO_NODE;
                                              _ -> ok
                                          end).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {dead_interval,
                name,
                tref}).

start_link(Name, HeartbeatInterval, DeadIntervalCount) ->
    gen_fsm:start_link(?MODULE, [Name, HeartbeatInterval, DeadIntervalCount], []).

?NODE_EVENT(heartbeat).

?NODE_EVENT(node_restarting).

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
    case load_status() of
        {ok, Status} ->
            case gproc:reg({n, l, Name}) of
                true ->
                    {next_state, Status, reset_timer(State)};
                false ->
                    {stop, shutdown, State}
            end;
        Error ->
            {stop, Error, State}
    end.

up(current_state, _From, State) ->
    {reply, up, up, State}.

crashed(current_state, _From, State) ->
    {reply, crashed, crashed, State}.

restarting(current_state, _From, State) ->
    {reply, restarting, restarting, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.

handle_info(heartbeat, up, State) ->
    {next_state, up, reset_timer(State)};
handle_info(hearbeat, crashed, State) ->
    {next_state, up, reset_timer(save_status(up, State))};
handle_info(heartbeat, restarting, State) ->
    {next_state, up, reset_timer(save_status(up, State))};
handle_info(crashed, up, State) ->
    {next_state, crashed, save_status(crashed, State)};
handle_info(restarting, up, State) ->
    {next_state, restarting, save_status(restarting, State)};
handle_info(restarting, crashed, State) ->
    {next_state, restarting, save_status(restarting, State)};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
load_status() ->
    {ok, up}.

save_status(Status, State) when is_atom(Status) ->
    save_status(status_code(Status), State);
save_status(Status, #state{name=Name}) ->
    case pushy_sql:create_node_status(Name, Status) of
        {conflict, _} -> pushy_sql:update_node_status(Name, Status);
        {error, Error} -> {error, Error}
    end.

%% Map status atom to valid integer before storing in db
status_code(up) ->
    1;
status_code(crashed) ->
    0.

reset_timer(#state{dead_interval=Interval, tref=undefined}=State) ->
    TRef = erlang:send_after(Interval, self(), no_heartbeats),
    State#state{tref=TRef};
reset_timer(#state{dead_interval=Interval, tref=TRef}=State) ->
    erlang:cancel_timer(TRef),
    TRef1 = erlang:send_after(Interval, self(), crashed),
    State#state{tref=TRef1}.
