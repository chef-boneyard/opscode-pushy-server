%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Node health statistics
%%% @end

-module(pushy_node_stats).

-record(metric, {node_pid :: pid(),
                 avg=down_threshold() * 2 :: float(),
                 last=os:timestamp() :: {pos_integer(), pos_integer(), pos_integer()},
                 heartbeats=1 :: pos_integer()}).

%% These two weights must total to 1.0
-define(HISTORY_WEIGHT, 0.85).
-define(NOW_WEIGHT, 0.15).

-export([init/0,
         heartbeat/1,
         scan/0]).

-spec init() -> atom() | ets:tid().
init() ->
    ets:new(?MODULE, [ordered_set, public, named_table, {keypos, 2},
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
                    ets:insert(?MODULE, reset(Node2)),
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

reset(Node) ->
    Node#metric{heartbeats=0}.

hb(#metric{heartbeats=Heartbeats}=Node) ->
    Node#metric{heartbeats=Heartbeats + 1, last=os:timestamp()}.

evaluate_node_health(#metric{avg=Avg, heartbeats=Heartbeats}=Node) ->
    case elapsed_intervals(Node) of
        0 ->
            {ok, Node};
        X ->
            Window = (decay_window() - 1) * X,
            WindowAvg = Heartbeats / Window,
            NAvg = (Avg * ?HISTORY_WEIGHT) + (WindowAvg * ?NOW_WEIGHT),
            Node1 = Node#metric{avg=NAvg, heartbeats=0},
            case NAvg < down_threshold() of
                true ->
                    {should_die, Node1};
                false ->
                    {reset, Node1}
            end
    end.

elapsed_intervals(#metric{last=TS}) ->
    ET = timer:now_diff(os:timestamp(), TS) div 1000,
    ET div heartbeat_interval().

heartbeat_interval() ->
    envy:get(pushy, heartbeat_interval, number).

down_threshold() ->
    envy:get(pushy, down_threshold, number).

decay_window() ->
    envy:get(pushy, decay_window, integer).
