%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

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

-type key() :: {atom(), binary()}.

-spec init() -> ok.
init() ->
    ets:new(?TABLE, [set,public,named_table, {read_concurrency, true}]),
    ok.

%% This exists to make eunit tests less painful to write.
-spec stop() -> ok.
stop() ->
    ets:delete(?TABLE).

%%%
%%% Key descriptor: {algorithm:atom, key:binary}
%%%
-spec generate_key(atom()) -> key().
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
