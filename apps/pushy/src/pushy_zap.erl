%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @doc
%%% Conducts background scans on node stats
%%% looking for zombie nodes
%%% @end

%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_zap).

%% This module implements the Zeromq ZAP protocol (http://rfc.zeromq.org/spec:27),  only validating
%% curve clients which have been added by add_key/1 within the previous 60 seconds.
%% There's no need for any particular security (i.e. obscuring of keys) in this code -- the
%% keys stored are all public.
-behaviour(gen_server).

-include("pushy.hrl").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1]).
-export([wait_for_start/0]).
-export([add_key/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {socket, %% :: erlzmq_socket(),
         clients :: dict:dict(binary(), integer())      % If this is too slow, change to orddict() ordered by client timestamp
        }).

% Client encrypted connection must be established within 60 seconds of sending in its public key (in the
% config request).
-define(EXPIRATION_TIME, 60).
% Run the expiration routine every 5 seconds.
-define(RUN_EXPIRE_TIME, 5000).

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

add_key(Key) ->
    gen_server:cast(?MODULE, {key, Key}).

wait_for_start() ->
    gen_server:call(?MODULE, wait_for_start).

init([#pushy_state{ctx=Ctx}]) ->
    lager:info("Starting ZAP server"),
    {ok, S} = erlzmq:socket(Ctx, [rep, {active, true}]),
    ok = erlzmq:bind(S, "inproc://zeromq.zap.01"),     % This address is mandated by the ZAP protocol
    timer:send_interval(?RUN_EXPIRE_TIME, expire),
    {ok, #state{socket=S, clients=dict:new()}}.

handle_call(wait_for_start, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({key, Key}, State) ->
    State1 = add_key_to_cache(Key, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(expire, State) ->
    State1 = expire_clients(State),
    {noreply, State1};
handle_info({zmq, _, Msg, Flags}, State) ->
    handle_zap_request(State, Msg, Flags),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlzmq:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% @doc Add the client public key to the set of validated keys.
%% of heartbeat intervals
add_key_to_cache(Key, State = #state{clients = Clients}) ->
    State#state{clients = dict:store(Key, time_to_integer(erlang:now()) + ?EXPIRATION_TIME, Clients)}.
    
time_to_integer({MegaS, S, _MicroS}) -> 1000000*MegaS + S.

expire_clients(State = #state{clients = Clients}) ->
    Now = time_to_integer(erlang:now()),
    NewClients = dict:filter(fun(_, T) -> T > Now end, Clients),
    State#state{clients = NewClients}.

handle_zap_request(State, Msg, [rcvmore]) ->
    S = State#state.socket,
    % XXX Wrap in an exception handler; return 500 if there's an exception

    % This is sent by zeromq, so we can pretty much assume the request is well-formed.
    Frames = pushy_messaging:receive_message_async(S, Msg),
    %% [<<"1.0">>,<<"1">>,<<>>,<<"33.33.33.1">>,<<>>,<<"CURVE">>,<<138,66,98,161,3,48,18,41,252,25,237,24,138,24,15,192,221,250,109,194,242,113,132,132,159,24,3,143,9,172,179,55>>]
    [<<"1.0">>, ReqId, _Domain, _Address, _Identity, <<"CURVE">>, Credentials] = Frames,
    {StatusCode, StatusText} = case dict:find(Credentials, State#state.clients) of
                                   {ok, _} -> {<<"200">>, <<"">>};
                                   error -> {<<"400">>, <<"No such client">>}
                               end,
    UserId = <<"">>,
    Metadata = <<"">>,
    Reply = [<<"1.0">>, ReqId, StatusCode, StatusText, UserId, Metadata],
    ok = pushy_messaging:send_message(S, Reply).
