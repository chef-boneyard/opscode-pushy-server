%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_org_events_sup).

-behaviour(supervisor).

-export([start_link/0, get_or_create_process/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    % XXX Possibly should be global if we want to scale across machines
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec get_or_create_process(binary()) -> pid().
get_or_create_process(Org) ->
    case supervisor:start_child(?SERVER, child_spec(Org)) of
        {error, {already_started, Child}} -> Child;
        {ok, Child} -> Child
    end.

child_spec(Org) ->
    {Org, {pushy_org_events, start_link, [Org]},
      transient, 5000, worker, [pushy_org_events]}.
    
%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    %If this call is uncommented, uncomment compile at top of file, too:
    %lager:trace_console([{module, pushy_node_state}], debug),
    {ok, {{one_for_one, 60, 120},
          []}}.
