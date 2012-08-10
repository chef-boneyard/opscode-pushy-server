%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_ema).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/2,
         reset_timer/1,
         tick/1,
         inc/2,
         value/1]).


-record(eavg, {acc = 0        :: integer(),
               avg = 0        :: float(),
               window = 0     :: integer(),
               tick_interval  :: integer(),
               tick_count = 0 :: integer()
              }).
-spec init(integer(), integer()) -> #eavg{}.
init(Window, Interval) when Window>0 ->
    EAvg = #eavg{acc=0, avg=0.0, window=Window, tick_interval=Interval},
    reset_timer(EAvg),
    EAvg.

reset_timer(#eavg{tick_interval=I}) ->
    ?debugVal(I),
    erlang:start_timer(I, self(), update_avg).

-spec tick(#eavg{}) -> #eavg{}.
tick(#eavg{acc=Acc, avg=Avg, window=Window, tick_count=C}=EAvg) ->
    NAvg = (Avg * (Window-1) + Acc)/Window,
    reset_timer(EAvg),
    EAvg#eavg{acc=0, avg=NAvg, tick_count=C+1}.

-spec inc(#eavg{}, number()) -> #eavg{}.
inc(#eavg{acc=Acc}=EAvg, Count) ->
    EAvg#eavg{acc=Acc+Count}.

-spec value(#eavg{}) -> float().
value(#eavg{avg=Avg}) ->
    Avg.

