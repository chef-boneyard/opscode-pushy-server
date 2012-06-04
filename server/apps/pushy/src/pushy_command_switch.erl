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
-include_lib("public_key/include/public_key.hrl").

-record(state,
        {command_sock}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    CommandAddress = pushy_util:make_zmq_socket_addr(command_port),

    error_logger:info_msg("Starting command mux listening on ~s~n.", [CommandAddress]),

    {ok, CommandSock} = erlzmq:socket(Ctx, [pull, {active, true}]),
    ok = erlzmq:bind(CommandSock, CommandAddress),
    State = #state{command_sock = CommandSock},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, _CommandSock, Header, [rcvmore]},
    #state{command_sock=_CommandSocket}=State) ->

    Body = pushy_util:read_body(),
    case catch pushy_util:do_authenticate_message(Header, Body) of
        ok ->
            
        {no_authn, bad_sig} ->
            error_logger:error_msg("Command message failed verification: header=~s~n", [Header])
    end,

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
