%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2011-2012 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_key_manager).

-include_lib("eunit/include/eunit.hrl").


%%
%% Implementation note:
%% This is implemented as a lookup in an ets table instead of a gen_server
%% The rationale is that we will likely have multiple processes
%% looking up keys simultaneously. Each process will likely be
%% processing messages as fast as it can; the fan in into a gen_server
%% for key management would likely be a bottleneck.


%% TODO: add key lifetimes and forcible rekeying...
%%

%% API
-export([init/0,
         get_key/1,
         stop/0
        ]).

-define(TABLE, pushy_key_manager).
-define(KEYTYPE, hmac_sha256).

-type key() :: {?KEYTYPE, binary()}.

-spec init() -> ok.
init() ->
    _Tid = ets:new(?TABLE, [set,public,named_table, {read_concurrency, true}]),
    ok.

%% This exists to make eunit tests less painful to write.

%% TODO: conditionally compile and export this only when we're doing testing
-spec stop() -> ok.
stop() ->
    true = ets:delete(?TABLE),
    ok.

%%%
%%% Key descriptor: {algorithm:atom, key:binary}
%%%
%%
%% TODO: as currently coded, this function's argument is completely superfluous
generate_key(hmac_sha256) ->
    Key = pushy_util:rand_bytes(erlang:trunc(256/8)),
    {hmac_sha256, Key}.

-spec get_key(any()) -> key().
get_key(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Key}] ->
            Key;
        [] ->
           make_key(Name)
    end.


-spec make_key(any()) -> key().
make_key(Name) ->
    NewKey = generate_key(?KEYTYPE),
    case ets:insert_new(?TABLE, {Name, NewKey}) of
        false ->
            %% if we get anything but a keypair here something strange is happening...
            [{Name, Key}] = ets:lookup(?TABLE, Name),
            Key;
        true ->
            NewKey
    end.
