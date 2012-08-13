%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Mark Anderson <mark@opscode.com>
%%
%% @copyright 2012 Opscode Inc.
%% @end

%%
%% @doc simple FSM for tracking node heartbeats and thus up/down status
%%
-module(pushy_node_state_exp_tests).

-define(NODE, {<<"org">>, <<"thenode">>}).
-define(NS, pushy_node_state_exp).
-define(HB_INTERVAL, 100).
-define(DECAY_WINDOW, 4). %% 4 is friendly to base 2 float arith

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
             application:set_env(pushy, decay_window, ?DECAY_WINDOW),
             ok
     end,
     fun(_) ->
             ok
     end,
     [fun(_) ->
              %% Resource creation
              {"Start things up, check that we can find it, shut it down",
               fun() ->
                       Result = (catch ?NS:start_link(?NODE)),
                       ?debugVal(Result),
                       ?assertMatch({ok, _}, Result),
                       {ok, Pid} = Result,
                       ?assert(is_pid(Pid)),

                       NPid = gproc:lookup_pid({n,l,?NS:mk_gproc_name(?NODE)}),
                       ?assertEqual(NPid, Pid),

                       % cleanup code
                       erlang:unlink(Pid),
                       erlang:exit(Pid, kill)
               end}
      end,
      fun(_) ->
              {"Start it up, check that we can get state",
               fun() ->
                       {ok, Pid} = ?NS:start_link(?NODE),

                       erlang:unlink(Pid),
                       erlang:exit(Pid, kill)
               end}
      end
     ]}.

heartbeat_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
             {ok, Pid} = ?NS:start_link(?NODE),
             {Pid}
     end,
     fun({Pid}) ->
             erlang:unlink(Pid),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun({Pid}) ->
              %% Resource creation
              {"Check that we properly register ourselves",
               fun() ->

                       NPid = gproc:lookup_pid({n,l,?NS:mk_gproc_name(?NODE)}),
                       ?assertEqual(NPid, Pid)
               end}
      end,
      fun(_) ->
              {"Start it up, check that we can get state",
               fun() ->
                       V = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb",
               fun() ->
                       ?NS:heartbeat(?NODE),
                       V = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state",
               fun() ->
                       ?NS:heartbeat(?NODE),
                       V1= ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({down,_}, V2)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state until we drive it into 'up'",
               fun() ->
                       ?NS:heartbeat(?NODE),
                       V1 = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),

                       heartbeat_step(?NODE, ?HB_INTERVAL,6),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({up, _}, V2)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state until we drive it into 'up', then wait until it goes down",
               fun() ->
                       ?NS:heartbeat(?NODE),
                       V1 = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),

                       heartbeat_step(?NODE, ?HB_INTERVAL, 6),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({up, _}, V2),

                       timer:sleep(?HB_INTERVAL*7),
                       V3 = ?NS:current_state(?NODE),
                       ?assertMatch({down, _}, V3)
               end}
      end
     ]}.

watcher_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
             {ok, Pid} = ?NS:start_link(?NODE),
             {Pid}
     end,
     fun({Pid}) ->
             erlang:unlink(Pid),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun(_) ->
              {"Enable watchpoint",
               fun() ->
                       ?NS:start_watching(?NODE)
               end}
      end,
      fun(_) ->
              {"Disable watchpoint when none exists",
               fun() ->
                       ?NS:stop_watching(?NODE)
               end}
      end,
      fun(_) ->
              {"Start it up, start watch, do hb, check that we don't get a message w/o state change",
               fun() ->
                       ?NS:heartbeat(?NODE),
                       V1= ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({down,_}, V2),
                       Msg = receive
                                 X -> X
                             after
                                 0 -> none
                             end,
                       ?assertEqual(none, Msg)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, check state until we drive it into 'up'",
               fun() ->
                       ?NS:start_watching(?NODE),
                       ?NS:heartbeat(?NODE),
                       V1 = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),

                       heartbeat_step(?NODE, ?HB_INTERVAL, 6),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({up, _}, V2),
                       Msg = receive
                                 X -> X
                             after
                                 100 -> none
                             end,
                       ?assertEqual({node_heartbeat_event, ?NODE, up}, Msg)

               end}
      end,
      fun(_) ->
              {"Start it up, send hb, check state until we drive it into 'up', then wait until it goes down",
               fun() ->
                       ?NS:start_watching(?NODE),
                       ?NS:heartbeat(?NODE),
                       V1 = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.0}, V1),
                       timer:sleep(?HB_INTERVAL),

                       heartbeat_step(?NODE, ?HB_INTERVAL, 6),
                       V2 = ?NS:current_state(?NODE),
                       ?assertMatch({up, _}, V2),

                       timer:sleep(?HB_INTERVAL*7),
                       V3 = ?NS:current_state(?NODE),
                       ?assertMatch({down, _}, V3),

                       Msg1 = receive
                                 X1 -> X1
                             after
                                 100 -> none
                             end,
                       ?assertEqual({node_heartbeat_event, ?NODE, up}, Msg1),

                       Msg2 = receive
                                 X2 -> X2
                             after
                                 100 -> none
                             end,
                       ?assertEqual({node_heartbeat_event, ?NODE, down}, Msg2)

               end}
      end
     ]}.

logging_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
             {ok, Pid} = ?NS:start_link(?NODE),
             {Pid}
     end,
     fun({Pid}) ->
             erlang:unlink(Pid),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun(_) ->
              {"Enable logging",
               fun() ->
                       ?NS:set_logging(?NODE, verbose)
               end}
      end,
      fun(_) ->
              {"Disable logging",
               fun() ->
                       ?NS:set_logging(?NODE, normal)
               end}
      end
     ]}.


heartbeat_step(_Node, _SleepTime, 0) ->
    ok;
heartbeat_step(Node, SleepTime, Count) ->
    heartbeat_step(Node,SleepTime),
    heartbeat_step(Node, SleepTime, Count-1).

heartbeat_step(Node, SleepTime) ->
    ?NS:heartbeat(Node),
    _V = ?NS:current_state(Node),
    timer:sleep(SleepTime).
