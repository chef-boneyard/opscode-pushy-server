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
        {zeromq_receiver}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    ?debugVal("Starting node status tracker"),
    % expect "tcp://*:port_id"
    {ok, NodeStatusSocket} = application:get_env(pushy, node_status_socket),
    {ok, ZeromqReceiver} = erlzmq:socket(Ctx, pull),
    ok = erlzmq:bind(ZeromqReceiver, NodeStatusSocket),
    State = #state{zeromq_receiver = ZeromqReceiver},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, _ZeromqReceiver, Sender, [rcvmore]}, State) ->
    read_message(Sender),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

read_message(Sender) ->
    receive
        {zmq, _ZeromqReceiver, Target, [rcvmore]} ->
            read_message(Sender, Target, [])
    end.

read_message(Sender, Target, Body) ->
    receive
        {zmq, _ZeromqReceiver, BodyPart, [rcvmore]} ->
            read_message(Sender, Target, [BodyPart|Body]);
        {zmq, _ZeromqReceiver, BodyPart, []} ->
            Message = list_to_binary(lists:reverse([BodyPart|Body])),
            % TODO - decode into a record
            % TODO - handle bad JSON messages
            {[{_client, ClientName}]} = jiffy:decode(Message),
            pushy_node_state:heartbeat(binary_to_list(ClientName))
    end.
