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
         send_command/2,
         send_multi_command/2]).

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

%% TODO : figure out where this really should come from
-opaque dictionary() :: any().

-record(state,
        {command_sock,
         addr_node_map :: {dictionary(), dictionary()}
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

send_command(ClientName, Message) ->
    gen_server:cast(?MODULE, {send, ClientName, Message}).
send_multi_command(Clients, Message) ->
    gen_server:cast(?MODULE, {send_multi, Clients, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    CommandAddress = pushy_util:make_zmq_socket_addr(command_port),

    error_logger:info_msg("Starting command mux listening on ~s~n.", [CommandAddress]),

    {ok, CommandSock} = erlzmq:socket(Ctx, [router, {active, true}]),
    ok = erlzmq:bind(CommandSock, CommandAddress),
    State = #state{command_sock = CommandSock,
                   addr_node_map = addr_node_map_new()
                  },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, ClientName, Message}, #state{}=State) ->
    do_send(State, ClientName, Message),
    {noreply, State};
handle_cast({send_multi, ClientName, Message}, #state{}=State) ->
    do_send_multi(State, ClientName, Message),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, _CommandSock, Address, [rcvmore]},
            State) ->
    read_address_separator(),
    Header = read_header(),
    Body = pushy_util:read_body(),
    State1 = case catch pushy_util:do_authenticate_message(Header, Body) of
                 ok ->
                     process_message(State, Address, Header, Body);
                 {no_authn, bad_sig} ->
                     error_logger:error_msg("Command message failed verification: header=~s~n", [Header]),
                     State
             end,
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_send(#state{addr_node_map = AddrNodeMap,
               command_sock = CommandSocket},
        ClientName, Message) ->
    Address = addr_node_map_lookup_by_node(AddrNodeMap, ClientName),
    send_multipart(CommandSocket, Address, Message).

do_send_multi(State, Clients, Message) ->
    [ do_send(State,Client,Message) || Client <- Clients ].

process_message(State, Address, _Header, Body) ->
    case catch jiffy:decode(Body) of
        {Data} ->
            ClientName = ej:get(Data, {<<"client">>} ),
            _JobName = ej:get(Data, {<<"job_id">>} ),
            State2 = addr_node_map_update(State, Address, ClientName),
            %%% TODO SETH ADDS call here
            %%% 
            State2;
        {'EXIT', Error} ->
            error_logger:error_msg("Status message JSON parsing failed: body=~s, error=~s~n", [Body,Error]),
            State
    end.


%%
%% Utility functions; we should generalize these and move elsewhere.
%%

read_address_separator() ->
    receive
        {zmq, _Sock, <<>>, [rcvmore]} ->
            ok
    end.

read_header() ->
    receive
        {zmq, _Sock, HeaderFrame, [rcvmore]} ->
            HeaderFrame
    end.


addr_node_map_new() ->
    { dict:new(), dict:new() }.

kill_crossref(Forward, Backward, Key) ->
    case dict:find(Forward, Key) of
        {ok, OldValue} ->
            dict:erase(OldValue, Backward);
        error ->
            Backward
    end.

addr_node_map_update(#state{addr_node_map = AddrNodeMap} = State, Addr, Node) ->
    State#state{addr_node_map = addr_node_map_update(AddrNodeMap, Addr, Node)};
addr_node_map_update({AddrToNode, NodeToAddr}, Addr, Node) ->
    % purge any old references
    NodeToAddr1 = kill_crossref(AddrToNode, NodeToAddr, Addr),
    AddrToNode1 = kill_crossref(NodeToAddr, AddrToNode, Node),
    { dict:store(Addr, Node, AddrToNode1),
      dict:store(Node, Addr, NodeToAddr1) }.

addr_node_map_lookup_by_addr(#state{addr_node_map = AddrNodeMap}, Addr) ->
    addr_node_map_lookup_by_addr(AddrNodeMap, Addr);
addr_node_map_lookup_by_addr({AddrToNode, _}, Addr) ->
    dict:fetch(Addr, AddrToNode).

addr_node_map_lookup_by_node(#state{addr_node_map = AddrNodeMap}, Node) ->
    addr_node_map_lookup_by_addr(AddrNodeMap, Node);
addr_node_map_lookup_by_node({_, NodeToAddr}, Node) ->
    dict:fetch(Node, NodeToAddr).

send_multipart(Socket, Address, Message) ->
    erlzmq:send(Socket, Address, [sndmore]),
    erlzmq:send(Socket, <<>>, [sndmore]),
    send_multipart(Socket, Message).

send_multipart(_Socket, []) ->
    ok;
send_multipart(Socket, [Msg | []]) ->
    erlzqm:send(Socket, Msg, []);
send_multipart(Socket, [Head | Tail]) ->
    erlzmq:send(Socket, Head, [sndmore]),
    send_multipart(Socket, Tail).
