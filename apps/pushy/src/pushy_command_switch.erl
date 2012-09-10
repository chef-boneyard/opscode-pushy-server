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
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_receive/3,
         do_send/3,
         do_send_multi/3]).

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
-include_lib("pushy_metrics.hrl").

%% TODO : figure out where this really should come from
-opaque dictionary() :: any().

-record(state,
        {command_sock,
         addr_node_map :: {dictionary(), dictionary()},
         private_key
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

-spec send_command(node_ref(), binary()) -> ok.
send_command(NodeRef, Message) ->
    gen_server:cast(?MODULE, {send, NodeRef, Message}).

-spec send_multi_command([node_ref()], binary()) -> ok.
send_multi_command(NodeRefs, Message) ->
    gen_server:cast(?MODULE, {send_multi, NodeRefs, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#pushy_state{ctx=Ctx}]) ->
    CommandAddress = pushy_util:make_zmq_socket_addr(command_port),

    {ok, PrivateKey} = chef_keyring:get_key(pushy_priv),

    lager:info("Starting command mux listening on ~s.", [CommandAddress]),

    {ok, CommandSock} = erlzmq:socket(Ctx, [router, {active, true}]),
    ok = erlzmq:bind(CommandSock, CommandAddress),
    State = #state{command_sock = CommandSock,
                   addr_node_map = addr_node_map_new(),
                   private_key = PrivateKey
                  },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, NodeRef, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send, (State, NodeRef, Message))};
handle_cast({send_multi, NodeRefs, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send_multi, (State, NodeRefs, Message))};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, CommandSock, Frame, [rcvmore]}, State) ->
    {noreply, ?TIME_IT(?MODULE, do_receive, (CommandSock, Frame, State))};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_receive(CommandSock, Frame, State) ->
    %% TODO: This needs a more graceful way of handling message sequences. I really feel like we need to
    %% abstract out some more generalized routines to handle the message receipt process.
    [Address, Header, Body] = pushy_messaging:receive_message_async(CommandSock, Frame),
    lager:debug("Receiving message with address ~w", [Address]),

    lager:debug("Received message~n\tA ~p~n\tH ~s~n\tB ~s", [Address, Header, Body]),
    State1 = case catch ?TIME_IT(pushy_util, do_authenticate_message, (Header, Body)) of
                 ok ->
                     process_message(State, Address, Header, Body);
                 {no_authn, bad_sig} ->
                     lager:error("Command message failed verification: header=~s", [Header]),
                     State
             end,
    State1.

do_send(#state{addr_node_map = AddrNodeMap,
               command_sock = CommandSocket,
               private_key = PrivateKey}=State,
        NodeRef, Message) ->
    Address = addr_node_map_lookup_by_node(AddrNodeMap, NodeRef),
    pushy_messaging:send_message(CommandSocket, [Address,
        ?TIME_IT(pushy_util, signed_header_from_message, (PrivateKey, Message)),
        Message]),
    State.

do_send_multi(#state{addr_node_map = AddrNodeMap,
                     command_sock = CommandSocket,
                     private_key = PrivateKey}=State,
              NodeRefs, Message) ->
    FrameList = [
        ?TIME_IT(pushy_util, signed_header_from_message, (PrivateKey, Message)), Message],
    % TODO if you communicate with a node that doesn't exist, don't just fail, dude
    AddressList = [addr_node_map_lookup_by_node(AddrNodeMap, NodeRef) || NodeRef <- NodeRefs],
    pushy_messaging:send_message_multi(CommandSocket, AddressList, FrameList),
    State.

%% FIX: Take advantage of multiple function heads to make code easier to read
process_message(State, Address, _Header, Body) ->
    case catch jiffy:decode(Body) of
        {Data} ->
            {State2, NodeRef} = get_node_ref(State, Address, Data),
            JobId = ej:get({<<"job_id">>}, Data),
            Type = ej:get({<<"type">>}, Data),
            send_node_event(JobId, NodeRef, Type),
            State2;
        {'EXIT', Error} ->
            lager:error("Status message JSON parsing failed: body=~s, error=~s", [Body,Error]),
            State
    end.

send_node_event(JobId, NodeRef, <<"heartbeat">>) ->
    lager:debug("Received heartbeat for node ~p with job id ~p", [JobId, NodeRef]),
    pushy_node_state:heartbeat(NodeRef);
send_node_event(JobId, NodeRef, <<"ack_commit">>) ->
    pushy_job_state:ack_commit(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"nack_commit">>) ->
    pushy_job_state:nack_commit(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"ack_run">>) ->
    pushy_job_state:ack_run(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"nack_run">>) ->
    pushy_job_state:nack_run(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"complete">>)->
    pushy_job_state:completed(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"aborted">>) ->
    pushy_job_state:aborted(JobId, NodeRef);
send_node_event(JobId, NodeRef, undefined) ->
    lager:error("Status message for job ~p and node ~p was missing type field!~n", [JobId, NodeRef]);
send_node_event(JobId, NodeRef, UnknownType) ->
    lager:error("Status message for job ~p and node ~p had unknown type ~p!~n", [JobId, NodeRef, UnknownType]).



get_node_ref(State, Address, Data) ->
    % This essentially debug code.
    _ClientName = ej:get({<<"client">>}, Data),
    OrgName  = ej:get({<<"org">>}, Data),
    NodeName = ej:get({<<"node">>}, Data),
    OrgId = pushy_object:fetch_org_id(OrgName),
    NodeRef = {OrgId, NodeName},
    %% Every time we get a message from a node, we update it's Addr->Name mapping entry
    State2 = addr_node_map_update(State, Address, NodeRef),
    {State2, NodeRef}.

%%
%% Utility functions; we should generalize these and move elsewhere.
%%

addr_node_map_new() ->
    { dict:new(), dict:new() }.

kill_crossref(Forward, Backward, Key) ->
    case dict:find(Key, Forward) of
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


