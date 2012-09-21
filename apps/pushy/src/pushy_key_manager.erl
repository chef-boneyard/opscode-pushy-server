%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_key_manager).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/0,
         get_key/1,
         generate_key/1,
         rekey/1
        ]).

-define(TABLE, pushy_key_manager).
-define(KEYTYPE, hmac_sha256).

-type key() :: {atom(), binary()}.

-spec init() -> ok.
init() ->
    ets:new(?TABLE, [set,public,named_table, {read_concurrency, true}]),
    ok.

%%%
%%% Key descriptor: {algorithm:atom, key:binary}
%%%
-spec generate_key(atom()) -> key().
generate_key(hmac_sha256) ->
    Key = pushy_util:rand_bytes(256/8),
    {hmac_sha256, Key}.

-spec get_key(any()) -> key().
get_key(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Key}] ->
            Key;
        [] ->
           rekey(Name)
    end.


-spec rekey(any()) -> key().
rekey(Name) ->
    NewKey = generate_key(?KEYTYPE),
    ets:insert(?TABLE, {Name, NewKey}),
    NewKey.
