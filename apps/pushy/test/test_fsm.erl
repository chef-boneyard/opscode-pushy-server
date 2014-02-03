%%%-------------------------------------------------------------------
%%% @author James Casey
%%% @doc Simple FSM for testing Job Monitor
%%%
%%% @end
%%%-------------------------------------------------------------------

%% @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
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

-module(test_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         shutdown/1,
         crash_me/0,
         stop_me/0]).

%% gen_fsm callbacks
-export([init/1,

         running/2,

         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

shutdown(Pid) ->
  exit(Pid, shutdown).

stop_me() ->
  gen_fsm:send_event(?MODULE, stop_me).

crash_me() ->
  gen_fsm:send_event(?MODULE, crash_me).

%%
%% FSM Callbacks
%%

%% This simulates normal shutdown of the pushy_job_state
running(stop_me, State) ->
    {stop, {shutdown, complete}, State}.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {ok, running, #state{}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
