%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Mux-demux for commands to clients and responses from clients
%%% @end

-module(pushy_command_switch).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         send_raw/1]).

%% ------------------------------------------------------------------
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_receive/3,
         do_send_raw/2]).

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

-include("pushy.hrl").
-include_lib("pushy_common/include/pushy_metrics.hrl").
-include_lib("pushy_common/include/pushy_messaging.hrl").


-define(PUSHY_MULTI_SEND_CROSSOVER, 100).

-record(state,
        {command_sock}).

-type addressed_message() :: [binary()]. % TODO Improve; it might be worth turning this into a tuple for better specificity.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

-spec send_raw([binary()]) -> ok.
send_raw(Message) ->
    gen_server:call(?MODULE, {send_raw, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#pushy_state{ctx=Ctx}]) ->
    CommandAddress = pushy_util:make_zmq_socket_addr(command_port),

    lager:info("Starting command mux listening on ~s.", [CommandAddress]),

    {ok, CommandSock} = erlzmq:socket(Ctx, [router, {active, true}]),
    ok = erlzmq:setsockopt(CommandSock, linger, 0),
    ok = erlzmq:bind(CommandSock, CommandAddress),
    State = #state{command_sock = CommandSock},
    {ok, State}.

handle_call({send_raw, Message}, _From, #state{}=State) ->
    NState = ?TIME_IT(?MODULE, do_send_raw, (State, Message)),
    {reply, ok, NState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, CommandSock, Frame, [rcvmore]}, State) ->
    {noreply, ?TIME_IT(?MODULE, do_receive, (CommandSock, Frame, State))};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{command_sock=CommandSock}) ->
    erlzmq:close(CommandSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_receive(CommandSock, Frame, State) ->
    %% TODO: This needs a more graceful way of handling message sequences. I really feel like we need to
    %% abstract out some more generalized routines to handle the message receipt process.
    case pushy_messaging:receive_message_async(CommandSock, Frame) of
        [_Address, _Header, _Body] = Message->
            lager:debug("RECV: ~s~nRECV: ~s~nRECV: ~s~n",
                       [pushy_tools:bin_to_hex(_Address), _Header, _Body]),
            pushy_node_state:recv_msg(Message),
            State;
        _Packets ->
            lager:debug("Received runt/overlength message with ~n packets~n", [length(_Packets)]),
            State
    end.

%%%
%%% Send a message to a single node
%%%
-spec do_send_raw(#state{}, addressed_message()) -> #state{}.
do_send_raw(#state{command_sock = CommandSocket}=State, RawMessage) ->
    [_Address, _Header, _Body] = RawMessage,
    lager:debug("SEND: ~s~nSEND: ~s~nSEND: ~s~n",
               [pushy_tools:bin_to_hex(_Address), _Header, _Body]),
    ok = pushy_messaging:send_message(CommandSocket, RawMessage),
    State.
