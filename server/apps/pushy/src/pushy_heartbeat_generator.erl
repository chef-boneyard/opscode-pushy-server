%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_heartbeat_generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         stats/0,
         heartbeat/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state,
        {zeromq_publisher,
         heartbeat_interval,
         beat_count}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

stats() ->
    gen_server:call(?SERVER, stats, infinity).

heartbeat() ->
    gen_server:cast(?SERVER, heartbeat).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    ?debugVal("Starting heartbeat generator"),
    {ok, Interval} = application:get_env(pushy, heartbeat_interval),
    % expect "tcp://*:port_id"
    {ok, HeartbeatSourceSocket} = application:get_env(pushy, server_heartbeat_socket),
    {ok, ZeromqPublisher} = erlzmq:socket(Ctx, pub),
    ok = erlzmq:bind(ZeromqPublisher, HeartbeatSourceSocket),
    State = #state{zeromq_publisher = ZeromqPublisher,
                   heartbeat_interval = Interval,
                   beat_count = 0
                  },
    timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, State}.

handle_call(stats, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(heartbeat, #state{zeromq_publisher=ZeromqPublisher, beat_count=Count}=State) ->
    % TODO - use a record
    % TODO - generate a timestamp; how does erchef do this?
    Msg = {[{server, ?SERVER}, {sequence, integer_to_list(Count)}, {timestamp, 0}, {type, heartbeat}]},
    ?debugVal(Msg),
    erlzmq:send(ZeromqPublisher, jiffy:encode(Msg)),
    {noreply, State#state{beat_count=Count+1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
