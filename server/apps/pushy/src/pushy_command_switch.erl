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
         send_command/3,
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

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

send_command(OrgName, NodeName, Message) ->
    gen_server:cast(?MODULE, {send, OrgName, NodeName, Message}).
send_multi_command(OrgName, Nodes, Message) ->
    gen_server:cast(?MODULE, {send_multi, OrgName, Nodes, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
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

handle_cast({send, OrgName, NodeName, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send, (State, OrgName, NodeName, Message))};
handle_cast({send_multi, OrgName, Nodes, Message}, #state{}=State) ->
    {noreply, ?TIME_IT(?MODULE, do_send_multi, (State, OrgName, Nodes, Message))};
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
    % error_logger:info_msg("Receiving message with address ~w~n", [Address]),
    [Address, Header, Body] = pushy_messaging:receive_message_async(CommandSock, Frame),

    % error_logger:info_msg("Received message~n\tA ~p~n\tH ~s~n\tB ~s~n", [Address, Header, Body]),
    State1 = case catch ?TIME_IT(pushy_util, do_authenticate_message, (Header, Body)) of
                 ok ->
                     process_message(State, Address, Header, Body);
                 {no_authn, bad_sig} ->
                     error_logger:error_msg("Command message failed verification: header=~s~n", [Header]),
                     State
             end,
    State1.

do_send(#state{addr_node_map = AddrNodeMap,
               command_sock = CommandSocket,
               private_key = PrivateKey}=State,
        Org, Node, Message) ->
    Address = addr_node_map_lookup_by_node(AddrNodeMap, {Org, Node}),
    push_messaging:send_message(CommandSocket, [Address,
        ?TIME_IT(pushy_util, signed_header_from_message, (PrivateKey, Message)),
        Message]),
    State.

do_send_multi(#state{addr_node_map = AddrNodeMap,
                     command_sock = CommandSocket,
                     private_key = PrivateKey}=State,
              Org, Nodes, Message) ->
    FrameList = [
        ?TIME_IT(pushy_util, signed_header_from_message, (PrivateKey, Message)), Message],
    AddressList = [addr_node_map_lookup_by_node(AddrNodeMap, {Org, Node}) || Node <- Nodes],
    pushy_messaging:send_message_multi(CommandSocket, AddressList, FrameList),
    State.

process_message(State, Address, _Header, Body) ->
    case catch jiffy:decode(Body) of
        {Data} ->
            Type = ej:get({<<"type">>}, Data ),

            % This essentially debug code.
            NodeName = ej:get({<<"node">>}, Data ),
            OrgName  = ej:get({<<"org">>}, Data ),
            _ClientName = ej:get({<<"client">>}, Data ),
            JobId = ej:get({<<"job_id">>}, Data, unknown ),

            %% Every time we get a message from a node, we update it's Addr->Name mapping entry
            State2 = addr_node_map_update(State, Address, {OrgName, NodeName}),
            case Type of
                % ready messages only are used to initialze
                <<"ready">> ->
                    lager:info("Node [~p] ready to RAWK this command party.", [NodeName]),
                    State2;
                <<"ack">> ->
                    pushy_job_runner:node_command_event(JobId, NodeName, ack),
                    State2;
                <<"nack">> ->
                    pushy_job_runner:node_command_event(JobId, NodeName, nack),
                    State2;
                <<"started">> ->
                    lager:info([{job_id, JobId}], "Node [~p] started running Job [~p]", [NodeName, JobId]),
                    pushy_job_runner:node_command_event(JobId, NodeName, started),
                    State2;
                <<"finished">> ->
                    lager:info([{job_id, JobId}], "Node [~p] finished running Job [~p]", [NodeName, JobId]),
                    pushy_job_runner:node_command_event(JobId, NodeName, finished),
                    State2;
                _Else ->
                    lager:info("I don't know anything about ~p", [Type]),
                    State2
            end;
        {'EXIT', Error} ->
            lager:error("Status message JSON parsing failed: body=~s, error=~s", [Body,Error]),
            State
    end.


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


