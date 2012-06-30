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

-export([create/4,
         update/4]).

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

-record(state,
        {foo}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(Org, Name, ActorId, Status) ->
    gen_server:cast(?MODULE, {create, Org, Name, ActorId, Status}).
update(Org, Name, ActorId, Status) ->
    gen_server:cast(?MODULE, {update, Org, Name, ActorId, Status}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    lager:info("Starting node status updater."),
    State = #state{foo=foo},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({create, Org, Name, ActorId, Status}, State) ->
    lager:info("Creating node ~s to status ~s", [Name, Status]),
    NodeStatus = pushy_object:new_record(pushy_node_status, Org,
                                         [{<<"node">>, Name},{<<"type">>, Status}]),
    NewState = case pushy_object:create_object(create_node_status, NodeStatus, ActorId) of
                   {ok, _} -> State;
                   {conflict, _} ->
                       pushy_object:update_object(update_node_status, NodeStatus, ActorId),
                       State;
                   {error, _Error} ->
                       %% TODO: retry here or something, we can't just drop the status on the floor...
                       lager:info("Error updating node ~s to status ~s", [Name, Status]),
                       State
               end,
    {noreply, NewState};
handle_cast({update, Org, Name, ActorId, Status}, State) ->
    lager:info("Updating node ~s to status ~s", [Name, Status]),
    NodeStatus = pushy_object:new_record(pushy_node_status, Org,
                                         [{<<"node">>, Name},{<<"type">>, Status}]),
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
