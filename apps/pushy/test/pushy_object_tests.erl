%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author James Casey <james@opscode.com>
%%
%% @end

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

-module(pushy_object_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CACHE, org_guid).

setup_cache() ->
    pushy_cache:init([{name, ?CACHE},
                      {ttl, 100000},
                      {max_size, 10000}]).

shutdown_cache() ->
    pushy_cache:close(?CACHE).

%% Check the caching works on fetch_object ID
fetch_object_id_test_() ->
    MockedModules = [pushy_org],
    {foreach,
     fun() ->
         setup_cache(),
         meck:new(MockedModules, [])
     end,
     fun(_) ->
         shutdown_cache(),
         meck:unload()
     end,
     [{"A get on unknown value returns not_found",
       fun() ->
           meck:expect(pushy_org, fetch_org_id, fun(_OrgName) -> not_found end),
           Got = pushy_object:fetch_org_id("foo"),
           ?assertEqual(not_found, Got)
       end
      },
      {"A get on a cached value served from the cache",
       fun() ->
           meck:expect(pushy_org, fetch_org_id, fun(_OrgName) -> will_not_be_called end),
           Expected = <<"foo_id">>,
           pushy_cache:put(?CACHE, <<"foo">>, Expected),
           Got = pushy_object:fetch_org_id(<<"foo">>),
           ?assertEqual(Expected, Got)
        end
      },
      {"A get on a new value adds it to the cache",
       fun() ->
           Expected = <<"foo_id">>,
           meck:expect(pushy_org, fetch_org_id, fun(_OrgName) -> Expected end),
           pushy_cache:put(?CACHE, <<"foo">>, Expected),

           Got = pushy_object:fetch_org_id(<<"foo">>),
           ?assertEqual(Expected, Got),

           % Now check if a second call returns the right value
           meck:expect(pushy_org, fetch_org_id, fun(_OrgName) -> will_not_be_called end),
           ?assertEqual(will_not_be_called, pushy_org:fetch_org_id(<<"foo">>)),
           Got1 =  pushy_object:fetch_org_id(<<"foo">>),
           ?assertEqual(Expected, Got1)
        end
      }
     ]
    }.

