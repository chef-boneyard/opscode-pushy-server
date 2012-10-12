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
         send_command/3,
         send_multi_command/2,
         send_multi_command/3]).

%% ------------------------------------------------------------------
%% Private Exports - only exported for instrumentation
%% ------------------------------------------------------------------

-export([do_receive/3,
         do_send/4,
         do_send_multi/4]).

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

%% TODO : figure out where this really should come from
-opaque dictionary() :: any().

-record(state,
        {command_sock,
         addr_node_map :: {dictionary(), dictionary()},
         private_key
        }).

-type ejson() :: tuple(). % TODO Improve
-type signing_method() :: 'rsa2048_sha1' | 'hmac_sha256'.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

-spec send_command(node_ref(), ejson()) -> ok.
send_command(NodeRef, Message) ->
    send_command(hmac_sha256, NodeRef, Message).

-spec send_command(node_ref(), signing_method(), ejson()) -> ok.
send_command(Method, NodeRef, Message) ->
    gen_server:cast(?MODULE, {send, Method, NodeRef, Message}).


-spec send_multi_command([node_ref()], binary()) -> ok.
send_multi_command(NodeRefs, Message) ->
    send_multi_command(hmac_sha256, NodeRefs, Message).

send_multi_command(Method, NodeRefs, Message) ->
    gen_server:cast(?MODULE, {send_multi, Method, NodeRefs, Message}).

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

handle_cast({send, Method, NodeRef, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send, (State, Method, NodeRef, Message))};
handle_cast({send_multi, Method, NodeRefs, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send_multi, (State, Method, NodeRefs, Message))};
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
    case pushy_messaging:receive_message_async(CommandSock, Frame) of
        [Address, Header, Body] ->
            lager:debug("Receiving message with address ~w", [Address]),
            lager:debug("Received message~n\tA ~p~n\tH ~s~n\tB ~s", [Address, Header, Body]),

            KeyFetch = fun(M, EJson) -> get_key_for_method(M, State, EJson) end,
            State1 = try ?TIME_IT(pushy_messaging, parse_message, (Address, Header, Body, KeyFetch)) of
                         {ok, #pushy_message{} = Msg} ->
                             process_message(State, Msg);
                         {error, #pushy_message{validated=bad_sig}} ->
                             lager:error("Command message failed verification: header=~s", [Header]),
                             State
                     catch
                         error:Error ->
                             Stack = erlang:get_stacktrace(),
                             lager:error("Command message parser failed horribly: header=~w~nstack~s", [Error, Stack]),
                             State;
                         Error ->
                             Stack = erlang:get_stacktrace(),
                             lager:error("Command message parser failed horribly: header=~w~nstack~s", [Error, Stack]),
                             State
                     end,
            State1;
        _Packets ->
            lager:debug("Received runt/overlength message with ~n packets~n", [length(_Packets)]),
            State
    end.

do_send(#state{addr_node_map = AddrNodeMap,
               command_sock = CommandSocket}=State,
        Method, NodeRef, Message) ->
    Key = get_key_for_method(Method, State, NodeRef),
    Address = addr_node_map_lookup_by_node(AddrNodeMap, NodeRef),
    Packets = ?TIME_IT(pushy_messaging, make_message, (proto_v2, Method, Key, Message)),
    ok = pushy_messaging:send_message(CommandSocket, [Address | Packets]),
    State.

%% This is broken!!! sort it out and fix it!

get_key_for_method(rsa2048_sha1, #state{private_key = PrivateKey}, _EJson) ->
    {ok, PrivateKey};
get_key_for_method(hmac_sha256, _State, EJson) ->
    NodeRef = get_node_ref(EJson),
    {hmac_sha256, Key} = pushy_key_manager:get_key(NodeRef),
    {ok, Key}.



%%
%%
%%
do_send_multi(#state{addr_node_map = AddrNodeMap,
                     command_sock = Socket} = State, 
              hmac_sha256 = Method, NodeRefs, Message) ->
    N2Addr = fun(NodeRef) ->
                     addr_node_map_lookup_by_node(AddrNodeMap, NodeRef)
             end,
    N2Key = fun(hmac_sha256, NodeRef) ->
                    {hmac_sha256, Key} = pushy_key_manager:get_key(NodeRef),
                    Key
            end,
    pushy_messaging:make_send_message_multi(Socket, proto_v2, Method,
                                            NodeRefs, Message, N2Addr, N2Key),
    State;
do_send_multi(#state{addr_node_map = AddrNodeMap,
                     command_sock = Socket,
                     private_key = PrivateKey}=State,
              rsa2048_sha1 = Method, NodeRefs, Message) ->
    N2Addr = fun(NodeRef) ->
                     addr_node_map_lookup_by_node(AddrNodeMap, NodeRef)
             end,
    N2Key = fun(rsa2048_sha1, _) ->
                    PrivateKey
            end,
    pushy_messaging:make_send_message_multi(Socket, proto_v2, Method,
                                            NodeRefs, Message, N2Addr, N2Key),
    State.

%% FIX: Take advantage of multiple function heads to make code easier to read
process_message(State, #pushy_message{address=Address, body=Data}) ->
    {State2, NodeRef} = get_node_ref(State, Address, Data),
    JobId = ej:get({<<"job_id">>}, Data),
    Type = ej:get({<<"type">>}, Data),
    send_node_event(JobId, NodeRef, Type),
    State2.

send_node_event(JobId, NodeRef, <<"heartbeat">>) ->
    lager:debug("Received heartbeat for node ~p with job id ~p", [JobId, NodeRef]),
    pushy_node_state:heartbeat(NodeRef);
send_node_event(JobId, NodeRef, <<"ack_commit">>) ->
    pushy_job_state:node_ack_commit(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"nack_commit">>) ->
    pushy_job_state:node_nack_commit(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"ack_run">>) ->
    pushy_job_state:node_ack_run(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"nack_run">>) ->
    pushy_job_state:node_nack_run(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"complete">>)->
    pushy_job_state:node_complete(JobId, NodeRef);
send_node_event(JobId, NodeRef, <<"aborted">>) ->
    pushy_job_state:node_aborted(JobId, NodeRef);
send_node_event(JobId, NodeRef, undefined) ->
    lager:error("Status message for job ~p and node ~p was missing type field!~n", [JobId, NodeRef]);
send_node_event(JobId, NodeRef, UnknownType) ->
    lager:error("Status message for job ~p and node ~p had unknown type ~p!~n", [JobId, NodeRef, UnknownType]).


get_node_ref(Data) ->
    %% This essentially debug code.
    _ClientName = ej:get({<<"client">>}, Data),
    OrgName  = ej:get({<<"org">>}, Data),
    NodeName = ej:get({<<"node">>}, Data),
    %% TODO: Clean up usage and propagation of org name vs org guid (OC-4351)
    OrgId = pushy_object:fetch_org_id(OrgName),
    {OrgId, NodeName}.

get_node_ref(State, Address, Data) ->
    NodeRef = get_node_ref(Data),
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
    %% purge any old references
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
