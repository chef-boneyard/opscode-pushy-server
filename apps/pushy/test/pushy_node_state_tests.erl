%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Mark Anderson <mark@opscode.com>
%%
%% @end

%%
%% @doc simple FSM for tracking node heartbeats and thus up/down status
%%
%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
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

-module(pushy_node_state_tests).

-define(NODE, {<<"org">>, <<"thenode">>}).
-define(ADDR, <<"AAAAAAA">>).
-define(INC_ID, <<"12345">>).
-define(NS, pushy_node_state).
-define(GPROC_NAME, {heartbeat,<<"org">>,<<"thenode">>}).

-define(HB_INTERVAL, 100).
-define(DECAY_WINDOW, 4). %% 4 is friendly to base 2 float arith
-define(DOWN_THRESHOLD, 0.25).

-include_lib("eunit/include/eunit.hrl").

-define(ASSERT_AVAILABLE(Node), ?assertMatch({online, {available, _}}, ?NS:status(Node)) ).
-define(ASSERT_UNAVAILABLE(Node), ?assertMatch({online, {unavailable, _}}, ?NS:status(Node)) ).
-define(ASSERT_OFFLINE(Node), ?assertMatch({offline, {unavailable, _}}, ?NS:status(Node)) ).

basic_setup() ->
    test_util:start_apps(),

    meck:new([pushy_command_switch, pushy_object], []),
    meck:expect(pushy_command_switch, send,
                fun(_Message) -> ok end),
    meck:expect(pushy_object, fetch_org_id,
                fun(X) -> X end),
    application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
    application:set_env(pushy, decay_window, ?DECAY_WINDOW),
    application:set_env(pushy, down_threshold, ?DOWN_THRESHOLD),

    pushy_key_manager:init(),
    pushy_node_stats:init(),
    ok.


basic_cleanup() ->
    pushy_key_manager:stop(),
    pushy_node_stats:stop(),
    meck:unload(pushy_object),
    meck:unload(pushy_command_switch).

mk_message_body({Org,Node}, Type, IncarnationId) ->
    {[{<<"node">>, Node},
      {<<"client">>, <<"private-chef.opscode.piab">>},
      {<<"org">>, Org},
      {<<"type">>,  atom_to_binary(Type, utf8)},
      {<<"incarnation_id">>, IncarnationId},
      {<<"job_state">>, <<"idle">>},
      {<<"job_id">>, <<"null">>},
      {<<"sequence">>, 2} ]}.

send_message(Node, Type, IncarnationId) ->
    {hmac_sha256, Key} = pushy_key_manager:get_key(Node),
    Body = mk_message_body(Node, Type, IncarnationId),
    Body2 = pushy_messaging:insert_timestamp_and_sequence(Body, 0),
    Json = jiffy:encode(Body2),
    Header = pushy_messaging:make_header(proto_v2, hmac_sha256, Key, Json),
    Message = [?ADDR, Header, Json],
    pushy_node_state:recv_msg(Message).

message_test_() ->
    {foreach,
     fun() ->
             basic_setup(),
             {ok, Pid} = ?NS:start_link(?NODE, ?ADDR, ?INC_ID),
             erlang:unlink(Pid),
             {Pid}
     end,
     fun({Pid}) ->
             basic_cleanup(),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun(_) ->
              {"Send heartbeats to a node",
               fun() ->
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, heartbeat, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       send_message(?NODE, aborted, ?INC_ID),
                       timer:sleep(?HB_INTERVAL),
                       ?ASSERT_AVAILABLE(?NODE)
               end
              }
      end
     ]}.


init_test_() ->
    {foreach,
     fun basic_setup/0,
     fun(_) ->
             basic_cleanup(),
             ok
     end,
     [fun(_) ->
              %% Resource creation
              {"Start things up, check that we can find it, shut it down",
               fun() ->
                       Result = (catch ?NS:start_link(?NODE, ?ADDR, ?INC_ID)),
                       ?assertMatch({ok, _}, Result),
                       {ok, Pid} = Result,
                       ?assert(is_pid(Pid)),

                       NPid = pushy_node_state_sup:get_process(?NODE),
                       ?assertEqual(NPid, Pid),

                       % cleanup code
                       erlang:unlink(Pid),
                       erlang:exit(Pid, kill)
               end}
      end,
      fun(_) ->
              {"Start it up, check that we can get state",
               fun() ->
                       {ok, Pid} = ?NS:start_link(?NODE, ?ADDR, ?INC_ID),

                       erlang:unlink(Pid),
                       erlang:exit(Pid, kill)
               end}
      end
     ]}.

heartbeat_test_() ->
    {foreach,
     fun() ->
             basic_setup(),
             {ok, Pid} = ?NS:start_link(?NODE, ?ADDR, ?INC_ID),
             erlang:unlink(Pid),
             {Pid}
     end,
     fun({Pid}) ->
             basic_cleanup(),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun({Pid}) ->
              %% Resource creation
              {"Check that we properly register ourselves",
               fun() ->

                       NPid = gproc:lookup_pid({n,l,?GPROC_NAME}),
                       ?assertEqual(NPid, Pid)
               end}
      end,
      fun(_) ->
              {"Start it up, check that we can get state",
               fun() ->
                       V = ?NS:status(?NODE),
                       ?assertMatch({online,{unavailable, none}}, V)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb",
               fun() ->
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       V = ?NS:status(?NODE),
                       ?assertMatch({online,{unavailable, none}}, V)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state",
               fun() ->
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       V1= ?NS:status(?NODE),
                       ?assertEqual({online, {unavailable, none}}, V1),
                       timer:sleep(?HB_INTERVAL),
                       V2 = ?NS:status(?NODE),
                       ?assertMatch({online, {unavailable, none}}, V2)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state until we drive it into 'up'",
               fun() ->
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       ?ASSERT_UNAVAILABLE(?NODE),
                       timer:sleep(?HB_INTERVAL),

                       %% Drive it up
                       heartbeat_step(?ADDR, ?HB_INTERVAL, 6),
                       ?NS:aborted(?NODE),
                       ?ASSERT_AVAILABLE(?NODE)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, sleep, check state until we drive it into 'up', then wait until it goes down",
               fun() ->
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       ?ASSERT_UNAVAILABLE(?NODE),
                       timer:sleep(?HB_INTERVAL),

                       %% Drive it up
                       heartbeat_step(?ADDR, ?HB_INTERVAL, 6),
                       ?NS:aborted(?NODE),
                       ?ASSERT_AVAILABLE(?NODE),

                       %% Drive it down, scan, then wait a moment and check
                       timer:sleep(?HB_INTERVAL*7),
                       pushy_node_stats:scan(),
                       timer:sleep(?HB_INTERVAL),
                       ?ASSERT_OFFLINE(?NODE)
               end}
      end
     ]}.

watcher_test_() ->
    {foreach,
     fun() ->
             basic_setup(),
             {ok, Pid} = ?NS:start_link(?NODE, ?ADDR, ?INC_ID),
             erlang:unlink(Pid),
             {Pid}
     end,
     fun({Pid}) ->
             basic_cleanup(),
             erlang:exit(Pid, kill),
             ok
     end,
     [fun(_) ->
              {"Enable watchpoint",
               fun() ->
                       ?NS:watch(?NODE)
               end}
      end,
      fun(_) ->
              {"Start it up, start watch, do hb, check that we don't get a message w/o state change",
               fun() ->
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       V1= ?NS:status(?NODE),
                       ?assertEqual({online, {unavailable, none}}, V1),
                       timer:sleep(?HB_INTERVAL),
                       V2 = ?NS:status(?NODE),
                       ?assertEqual({online, {unavailable, none}}, V2),
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
                       ?NS:watch(?NODE),
                       ?NS:heartbeat(?ADDR, ?INC_ID),
                       heartbeat_step(?ADDR, ?HB_INTERVAL, 6),
                       ?NS:aborted(?NODE),
                       ?ASSERT_AVAILABLE(?NODE)
               end}
      end,
      fun(_) ->
              {"Start it up, send hb, check state until we drive it into 'up', then wait until it goes down",
               fun() ->
                       ?NS:watch(?NODE),
                       heartbeat_step(?ADDR, ?HB_INTERVAL, 6),
                       ?NS:aborted(?NODE),
                       ?ASSERT_AVAILABLE(?NODE),

                       timer:sleep(?HB_INTERVAL*7),
                       pushy_node_stats:scan(),
                       timer:sleep(?HB_INTERVAL),
                       ?ASSERT_OFFLINE(?NODE),

                       assertShutdown(?NODE)
               end}
      end
     ]}.

heartbeat_step(_Node, _SleepTime, 0) ->
    ok;
heartbeat_step(Node, SleepTime, Count) ->
    heartbeat_step(Node,SleepTime),
    heartbeat_step(Node, SleepTime, Count-1).

heartbeat_step(Node, SleepTime) ->
    ?NS:heartbeat(Node, ?INC_ID),
    _V = ?NS:status(Node),
    timer:sleep(SleepTime).

assertShutdown(Node) ->
    receive
        {state_change, Node, _CurrentState, shutdown} -> ok
    after
        100 -> throw("Node FSM did not shutdown")
    end.
