%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Conducts background scans on node stats
%%% looking for zombie nodes
%%% @end

-module(pushy_node_stats_scanner).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    random:seed(erlang:now()),
    {ok, none, wait_interval()}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    pushy_node_stats:scan(),
    {noreply, State, wait_interval()};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
wait_interval() ->
    HeartbeatInterval = envy:get(pushy, heartbeat_interval, number),
    (HeartbeatInterval * 4) + random:uniform(HeartbeatInterval).
