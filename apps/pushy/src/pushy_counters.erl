%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_counters).
%-behaviour(gen_server).
%-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([setup_aggregate_counters/0,
         setup_counters/1,
         get_aggregate_counters/0,
         state_change/2]).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_sql.hrl").

% Set up aggregate counters to track the state of the system
% This may not be the right place for this.
%
setup_aggregate_counters() ->
    [ gproc:add_local_aggr_counter(mk_state(S)) || S <- [total | ?HEARTBEAT_STATES] ].

get_aggregate_counters() ->
    [ {State, gproc:lookup_local_aggr_counter(mk_state(State))} || State <- [total | ?HEARTBEAT_STATES] ].


setup_counters(State) ->
    [ gproc:add_local_counter(mk_state(S), 0) || S <- [total | ?HEARTBEAT_STATES] ],
    update_counter(State,1),
    update_counter(total,1).

state_change(Old, New) ->
    update_counter(Old, -1),
    update_counter(New,  1).

update_counter(State, Incr) ->
    try
        gproc:update_counter({c,l,mk_state(State)},  Incr)
    catch
        error:X ->
            ?debugVal(gproc:lookup_local_counters(mk_state(State))),
            ?debugVal(X),
            ?debugVal(erlang:get_stacktrace())
    end.

mk_state(State) ->
    {node_state, State}.
