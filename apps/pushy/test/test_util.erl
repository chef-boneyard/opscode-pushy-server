%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Mark Anderson <mark@opscode.com>
%% @author John Keiser <john@opscode.com>
%%
%% @copyright 2012 Opscode Inc.
%% @end


%%
%% @doc glue code for tests
%%
-module(test_util).

-export([start_apps/0]).


-include_lib("eunit/include/eunit.hrl").

start_apps() ->
    application:start(gproc),
    application:start(folsom).
