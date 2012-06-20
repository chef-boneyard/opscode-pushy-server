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
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_send/1]).

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
-include("pushy.hrl").
-include_lib("pushy_metrics.hrl").


%% TODO: tighten typedefs up
-record(state,
        {heartbeat_sock :: any(),
         heartbeat_interval :: integer(),
         beat_count :: integer(),
         private_key :: any(),
         incarnation_id :: binary()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

stats() ->
    gen_server:call(?SERVER, stats, infinity).

heartbeat() ->
    gen_server:cast(?SERVER, heartbeat).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


init([#pushy_state{ctx=Ctx, incarnation_id=IncarnationId }]) ->
    lager:info("Starting heartbeat generator with incarnation id ~s.", [IncarnationId]),
    Interval = pushy_util:get_env(pushy, heartbeat_interval, fun is_integer/1),

    {ok, HeartbeatSock} = erlzmq:socket(Ctx, pub),
    {ok, PrivateKey} = chef_keyring:get_key(pushy_priv),

    HeartbeatAddress = pushy_util:make_zmq_socket_addr(server_heartbeat_port),

    lager:info("Starting heartbeat generator listening on ~s.",[HeartbeatAddress]),

    ok = erlzmq:bind(HeartbeatSock, HeartbeatAddress),
    State = #state{heartbeat_sock = HeartbeatSock,
                   heartbeat_interval = Interval,
                   beat_count = 0,
                   private_key = PrivateKey,
                   incarnation_id = IncarnationId
                  },
    timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, State}.

handle_call(stats, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(heartbeat, State) ->
    {noreply, ?TIME_IT(?MODULE, do_send, (State))};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_send(#state{heartbeat_sock=HeartbeatSock, beat_count=Count, private_key=PrivateKey}=State) ->
    {ok, Hostname} = inet:gethostname(),
    Msg = {[{server, list_to_binary(Hostname)},
            {timestamp, list_to_binary(httpd_util:rfc1123_date())},
            {type, heartbeat}]},
    % JSON encode message
    BodyFrame = jiffy:encode(Msg),

    % Send Header (including signed checksum)
    HeaderFrame = ?TIME_IT(pushy_util, signed_header_from_message, (PrivateKey, BodyFrame)),
    pushy_messaging:send_message(HeartbeatSock, [HeaderFrame, BodyFrame]),
    %?debugVal(BodyFrame),
    lager:debug("Heartbeat sent: header=~s,body=~s",[HeaderFrame, BodyFrame]),
    State#state{beat_count=Count+1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
