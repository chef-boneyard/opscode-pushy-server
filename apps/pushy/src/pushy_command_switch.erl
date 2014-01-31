%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @doc
%%% Mux-demux for commands to clients and responses from clients
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

-compile([{parse_transform, lager_transform}]).

-define(PUSHY_MULTI_SEND_CROSSOVER, 100).

-record(state,
        {r_sock,
         s_sock}).

%% TODO: some refactoring around this seems necessary.  First, it seems that this is
%% actually multiple messages (see pushy_job_state:do_send/3).  Turning it into a tuple for
%% better specificity might also be a good idea.
-type addressed_message() :: nonempty_list(binary()).

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
            lager:error("Unable to send message. No command switch processes found!"),
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
-spec do_send(#state{}, addressed_message()) -> #state{}.
do_send(#state{s_sock=Send}=State, RawMessage) ->
    [_Address, _Header, _Body] = RawMessage,
    lager:debug("SEND: ~s~nSEND: ~s~nSEND: ~s~n",
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
