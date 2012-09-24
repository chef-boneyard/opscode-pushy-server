%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_status_updater).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([create/3,
         update/3]).

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
-include_lib("pushy_common/include/pushy_metrics.hrl").

-record(state,
        {foo}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(NodeRef, ActorId, Status) ->
    gen_server:cast(?MODULE, {create, NodeRef, ActorId, Status}).
update(NodeRef, ActorId, Status) ->
    gen_server:cast(?MODULE, {update, NodeRef, ActorId, Status}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    lager:info("Starting node status updater."),
    State = #state{foo=foo},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({create, {OrgId, NodeName}=NodeRef, ActorId, Status}, State) ->
    lager:info("Creating node ~p with status ~p", [NodeRef, Status]),
    NodeStatus = pushy_object:new_record(pushy_node_status, OrgId,
                                         [{<<"node">>, NodeName},{<<"type">>, Status}]),
    NewState = case pushy_object:create_object(create_node_status, NodeStatus, ActorId) of
                   {ok, _} -> State;
                   {conflict, _} ->
                       pushy_object:update_object(update_node_status, NodeStatus, ActorId),
                       State;
                   {error, _Error} ->
                       %% TODO: retry here or something, we can't just drop the status on the floor...
                       lager:info("Error updating node ~p to status ~p", [NodeRef, Status]),
                       State
               end,
    {noreply, NewState};
handle_cast({update, {OrgId, NodeName}=NodeRef, ActorId, Status}, State) ->
    lager:info("Updating node ~p to status ~p", [NodeRef, Status]),
    NodeStatus = pushy_object:new_record(pushy_node_status, OrgId,
                                         [{<<"node">>, NodeName},{<<"type">>, Status}]),
    %% need to check that this update 'stuck'
    pushy_object:update_object(update_node_status, NodeStatus, ActorId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
