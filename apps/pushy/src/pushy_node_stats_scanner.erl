%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @doc
%%% Conducts background scans on node stats
%%% looking for zombie nodes
%%% @end

%% @copyright Copyright 2012-2012 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_node_stats_scanner).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

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

-record(state,
        {interval :: non_neg_integer()
        }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    _Seed = random:seed(erlang:now()),
    Interval = wait_interval(),
    lager:info("Starting pushy node cleanup (every ~p ms)", [Interval]),
    {ok, #state{interval=Interval}, Interval}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{interval = Interval} = State) ->
    pushy_node_stats:scan(),
    {noreply, State, Interval};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions


%% @doc The wait interval between scans, expressed as number
%% of heartbeat intervals
wait_interval() ->
    HeartbeatInterval = envy:get(pushy, heartbeat_interval, number),
    NumHeartbeats = envy:get(pushy, detect_offline_nodes_interval, number),
    HeartbeatInterval * NumHeartbeats.
