%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2011-2012 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
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
-include_lib("pushy_common/include/pushy_metrics.hrl").

-compile([{parse_transform, lager_transform}]).

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
    erlang:process_flag(trap_exit, true),
    lager:info("Starting heartbeat generator with incarnation id ~s.", [IncarnationId]),
    Interval = envy:get(pushy, heartbeat_interval, integer),

    {ok, HeartbeatSock} = erlzmq:socket(Ctx, pub),
    ok = erlzmq:setsockopt(HeartbeatSock, linger, 0),
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
    _Timer = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
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

terminate(_Reason, #state{heartbeat_sock=HeartbeatSock}) ->
    erlzmq:close(HeartbeatSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_send(#state{heartbeat_sock=HeartbeatSock, beat_count=Count, private_key=PrivateKey,
               incarnation_id=IncarnationId}=State) ->

    {ok, Hostname} = inet:gethostname(),
    Msg = {[{server, list_to_binary(Hostname)},
            {type, heartbeat},
            {incarnation_id, IncarnationId}
           ]},
    Msg2 = pushy_messaging:insert_timestamp_and_sequence(Msg, Count),
    Packets = ?TIME_IT(pushy_messaging, make_message, (proto_v2, rsa2048_sha1, PrivateKey, Msg2)),
    pushy_messaging:send_message(HeartbeatSock, Packets),
    %?debugVal(BodyFrame),
    lager:debug("Heartbeat sent: header=~s,body=~s", Packets),
    State#state{beat_count=Count+1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
