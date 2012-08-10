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

-define(NODE, "thenode").
-define(NS, pushy_node_state_exp).

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, 1000),
             ok
     end,
     fun(_) ->
             ok
     end,
     [fun(_) ->
              %% Resource creation
              {"Start things up, check that we can find it, shut it down",
               fun() ->
                       Result = ?NS:start_link(?NODE),
                       ?assertMatch({ok, _}, Result),
                       {ok, Pid} = Result,
                       ?assert(is_pid(Pid)),

                       NPid = gproc:lookup_pid({n,l,?NODE}),
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
             application:set_env(pushy, heartbeat_interval, 1000),
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

                       NPid = gproc:lookup_pid({n,l,?NODE}),
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
                       timer:sleep(1000),
                       V2 = ?NS:current_state(?NODE),
                       ?assertEqual({down,0.2}, V2)
               end}
      end
     ]}.

