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

-export([start_link/2,
         send/1,
         switch_processes_fun/0]).

%% ------------------------------------------------------------------
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_receive/3,
         do_send/2]).

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
        {r_sock,
         s_sock}).

-type addressed_message() :: [binary()]. % TODO Improve; it might be worth turning this into a tuple for better specificity.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState, Id) ->
    gen_server:start_link(?MODULE, [PushyState, Id], []).

-spec send([binary()]) -> ok.
send(Message) ->
    case select_switch() of
        {ok, Pid} ->
            gen_server:call(Pid, {send, Message}, infinity);
        Error ->
            pushy_logger:error("Unable to send message. No command switch processes found!"),
            Error
    end.

%% @doc Generate a function suitable for use with pushy_process_monitor that
%% lists the switch processes
-spec switch_processes_fun() -> fun(() -> [{binary(), pid()}]).
switch_processes_fun()->
    fun() ->
            case gproc:select({local, names}, [{{{n,l,{?MODULE, '_'}},'_','_'},[],['$_']}]) of
                [] ->
                   [];
                Switches ->
                    [extract_process_info(Switch) || Switch <- Switches]
            end
    end.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#pushy_state{ctx=Ctx}, Id]) ->
    {ok, Recv} = erlzmq:socket(Ctx, [pull, {active, true}]),
    {ok, Send} = erlzmq:socket(Ctx, [push, {active, false}]),
    [erlzmq:setsockopt(Sock, linger, 0) || Sock <- [Recv, Send]],
    ok = erlzmq:connect(Recv, ?PUSHY_BROKER_IN),
    ok = erlzmq:connect(Send, ?PUSHY_BROKER_OUT),
    State = #state{r_sock=Recv, s_sock=Send},
    true = gproc:reg({n, l, {?MODULE, Id}}),
    {ok, State}.

handle_call({send, Message}, _From, #state{}=State) ->
    NState = ?TIME_IT(?MODULE, do_send, (State, Message)),
    {reply, ok, NState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, CommandSock, Frame, [rcvmore]}, State) ->
    {noreply, ?TIME_IT(?MODULE, do_receive, (CommandSock, Frame, State))};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{r_sock=Recv, s_sock=Send}) ->
    erlzmq:close(Recv),
    erlzmq:close(Send),
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
            pushy_logger:debug("RECV: ~s~nRECV: ~s~nRECV: ~s~n",
                       [pushy_tools:bin_to_hex(_Address), _Header, _Body]),
            pushy_node_state:recv_msg(Message),
            State;
        _Packets ->
            pushy_logger:debug("Received runt/overlength message with ~n packets~n", [length(_Packets)]),
            State
    end.

%%%
%%% Send a message to a single node
%%%
-spec do_send(#state{}, addressed_message()) -> #state{}.
do_send(#state{s_sock=Send}=State, RawMessage) ->
    [_Address, _Header, _Body] = RawMessage,
    pushy_logger:debug("SEND: ~s~nSEND: ~s~nSEND: ~s~n",
               [pushy_tools:bin_to_hex(_Address), _Header, _Body]),
    ok = pushy_messaging:send_message(Send, RawMessage),
    State.

-spec select_switch() -> {ok, pid()} | {error, no_switches}.
select_switch() ->
    case gproc:select({local, names}, [{{{n,l,{?MODULE, '_'}},'_','_'},[],['$_']}]) of
        [] ->
            {error, no_switches};
        [Switch] ->
            {_, Pid, _} = Switch,
            {ok, Pid};
        Switches ->
            Pos = random:uniform(length(Switches)),
            {_, Pid, _} = lists:nth(Pos, Switches),
            {ok, Pid}
    end.

extract_process_info({{n,l, {Name, Id}}, Pid, _})  ->
    {list_to_binary(io_lib:format("~s_~B", [Name, Id])), Pid}.
