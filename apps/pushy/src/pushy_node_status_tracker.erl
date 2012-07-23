%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_status_tracker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_receive/3]).

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
-include_lib("public_key/include/public_key.hrl").

-include("pushy_sql.hrl").
-include("pushy.hrl").
-include("pushy_messaging.hrl").
-include("pushy_metrics.hrl").

-record(state,
        {status_sock,
         heartbeat_interval,
         dead_interval}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#pushy_state{ctx=Ctx}]) ->
    lager:info("Starting node status tracker."),
    StatusAddress = pushy_util:make_zmq_socket_addr(node_status_port),

    % find a smarter place for this
    pushy_counters:setup_aggregate_counters(),

    HeartbeatInterval = pushy_util:get_env(pushy, heartbeat_interval, fun is_integer/1),
    DeadInterval = pushy_util:get_env(pushy, dead_interval, fun is_integer/1),

    lager:info("Starting node status tracker listening on ~s.", [StatusAddress]),

    {ok, StatusSock} = erlzmq:socket(Ctx, [pull, {active, true}]),
    ok = erlzmq:bind(StatusSock, StatusAddress),
    State = #state{status_sock = StatusSock,
                   heartbeat_interval = HeartbeatInterval,
                   dead_interval = DeadInterval},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, StatusSock, Frame, [rcvmore]}, State) ->
    {noreply, ?TIME_IT(?MODULE, do_receive, (StatusSock, Frame, State))};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_receive(StatusSock, Frame,
    #state{heartbeat_interval=HeartbeatInterval,dead_interval=DeadInterval}=State) ->


%    [Header, Body] = pushy_messaging:receive_message_async(StatusSock, Frame),
%
%    case ?TIME_IT(pushy_util, do_authenticate_message, (Header, Body)) of
%        ok ->
%            send_heartbeat(Body, HeartbeatInterval, DeadInterval);
%        {no_authn, bad_sig} ->
%            lager:error("Status message failed verification: header=~s", [Header])
%    end,
    Packet = pushy_messaging:parse_receive_message(StatusSock, Frame, undefined),
    case Packet#message.validated of
        ok ->
            send_heartbeat(Packet#message.body, HeartbeatInterval, DeadInterval);
        Validation ->
            lager:error("Heartbeat message failed verification: ~s", [Validation])
    end,
    State.

send_heartbeat(BodyFrame, HeartbeatInterval, DeadInterval) when is_binary(BodyFrame) ->
    case catch jiffy:decode(BodyFrame) of
        {Hash} ->
            send_heartbeat(Hash, HeartbeatInterval, DeadInterval);
        {'EXIT', Error} ->
            lager:error("Status message JSON parsing failed: body=~s, error=~s", [BodyFrame,Error])
    end;
send_heartbeat(Hash, HeartbeatInterval, DeadInterval) ->
    NodeName = proplists:get_value(<<"node">>, Hash),
    NodeState = proplists:get_value(<<"state">>, Hash),

    case catch pushy_node_state:heartbeat({heartbeat, NodeName, NodeState}) of
        {error, no_node} ->
            pushy_node_state_sup:new(NodeName, HeartbeatInterval, DeadInterval),
            send_heartbeat(Hash, HeartbeatInterval, DeadInterval);
        ok ->
            lager:debug("Heartbeat received from: ~s", [NodeName]),
            ok
    end.

