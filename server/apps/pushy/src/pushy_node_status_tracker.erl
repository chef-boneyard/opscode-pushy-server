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
        {status_sock}).

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
    {ok, StatusAddress} = application:get_env(pushy, node_status_socket),
    {ok, StatusSock} = erlzmq:socket(Ctx, [pull, {active, true}]),
    ok = erlzmq:bind(StatusSock, StatusAddress),
    State = #state{status_sock = StatusSock},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, _StatusSock, Sig, [rcvmore]}, State) ->
    read_message(Sig),
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

read_message(Sig) ->
    ?debugVal(Sig),
    receive
        {zmq, _StatusSock, Body, []} ->
            ?debugVal(Body),
            % TODO - verify sig
            case catch jiffy:decode(Body) of
              {Hash} ->
                NodeName = proplists:get_value(<<"node">>, Hash),
                pushy_node_state:heartbeat(NodeName);
              {'EXIT', _Error} ->
                %% Log error and move on
                error_logger:info_msg("Bad status message received: ~s~n", [Body])
            end
    end.
