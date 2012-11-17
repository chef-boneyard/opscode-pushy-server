%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Node health statistics
%%% @end

-module(pushy_node_stats).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(metric, {node_pid :: pid(),
                 avg=down_threshold() * 2 :: float(),
                 interval_start=now_as_int() :: pos_integer(),
                 heartbeats=1 :: pos_integer()}).

%% These two weights must total to 1.0
-define(NOW_WEIGHT, (1.0/decay_window())).
-define(HISTORY_WEIGHT, (1.0-?NOW_WEIGHT)).

-define(MEGA, 1000000). %% because I can't count zeros reliably

-export([init/0,
         heartbeat/1,
         scan/0]).

-spec init() -> atom() | ets:tid().
init() ->
    ets:new(?MODULE, [set, public, named_table, {keypos, 2},
                      {write_concurrency, true}, {read_concurrency, true}]).

-spec heartbeat(pid()) -> ok | should_die.
heartbeat(NodePid) ->
    case ets:lookup(?MODULE, NodePid) of
        [] ->
            ets:insert_new(?MODULE, #metric{node_pid=NodePid}),
            ok;
        [Node] ->
            Node1 = hb(Node),
            case evaluate_node_health(Node1) of
                {reset, Node2} ->
                    ets:insert(?MODULE, Node2),
                    ok;
                {ok, Node2} ->
                    ets:insert(?MODULE, Node2),
                    ok;
                {should_die, _Node2} ->
                    ets:delete_object(?MODULE, Node1),
                    should_die
            end
    end.

-spec scan() -> ok.
scan() ->
    ets:safe_fixtable(?MODULE, true),
    try
        scan(ets:first(?MODULE))
    after
        %% Make sure we unfix the table even
        %% if an error was thrown
        ets:safe_fixtable(?MODULE, false)
    end.

scan('$end_of_table') ->
    ok;
scan(NodePid) ->
    [Node] = ets:lookup(?MODULE, NodePid),
    case evaluate_node_health(Node) of
        {ok, Node1} ->
            ets:insert(?MODULE, Node1),
            ok;
        {reset, Node1} ->
            ets:insert(?MODULE, Node1),
            ok;
        {should_die, _Node1} ->
            ets:delete_object(?MODULE, Node),
            NodePid ! should_die
    end,
    scan(ets:next(?MODULE, NodePid)).

hb(#metric{}=Node) ->
    Node1 = maybe_advance_interval(Node),
    Node1#metric{heartbeats=Node1#metric.heartbeats + 1}.


evaluate_node_health(Node) ->
    Node1 = maybe_advance_interval(Node),
    #metric{node_pid=Pid, avg=NAvg} = Node1,
    case NAvg < down_threshold() of
        true ->
            lager:debug("Killing Node: ~p~n", [Pid]),
            {should_die, Node1};
        false ->
            lager:debug("Resetting Node: ~p~n", [Pid]),
            {reset, Node1}
    end.

heartbeat_interval() ->
    %% Heartbeat interval is specified in milliseconds, but we keep time in microseconds.
    envy:get(pushy, heartbeat_interval, integer) * 1000.

down_threshold() ->
    envy:get(pushy, down_threshold, number).

decay_window() ->
    envy:get(pushy, decay_window, integer).

%% Advance the heartbeat interval to include the current time.
%%
%% If it has been a while since we updated (as when we haven't received a heartbeat in a
%% while), there may be several empty intervals between the last updated interval and the one we
%% are in now.
%%
hb_step(#metric{interval_start=StartI} = M, Now, Interval)
  when Now < StartI + Interval ->
    %% If the current interval contains our current time, we do nothing
    M;
hb_step(#metric{avg=Avg, interval_start=StartI, heartbeats=Hb} = M, Now, Interval) ->
    NextI = StartI + Interval,
    %% If we are in a new interval we fold the old hb counter into the moving
    %% average and advance the window marker.
    %% NOTE: This could computed directly without iteration, but I think this is
    %% clearer expressed this way.
    NAvg = (Avg * ?HISTORY_WEIGHT) + (Hb * ?NOW_WEIGHT),
    hb_step(M#metric{avg=NAvg, interval_start=NextI, heartbeats=0}, Now, Interval).

now_as_int() ->
    {M, S, U} = os:timestamp(),
    ((M*?MEGA) + S) * ?MEGA + U.

