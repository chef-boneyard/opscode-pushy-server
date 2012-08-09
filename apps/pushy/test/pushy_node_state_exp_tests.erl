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

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
     fun() ->
             test_util:start_apps(),
             application:set_env(pushy, heartbeat_interval, 1),
             ok
     end,
     fun(_) ->
             ok
     end,
     [fun(_) ->
              %% Resource creation
              {"Start things up",
               fun() ->
                       Result = pushy_node_state_exp:start_link(?NODE),
                       ?debugVal(Result),
                       ?assertMatch({ok, _}, Result),
                       {ok, Pid} = Result,
                       ?assert(is_pid(Pid))
               end}
      end,
      fun(_) ->
              {"Simple group create by non superuser",
               fun() ->
%                       error(die),
%                       {ok, Actor} = chef_authz:create_resource(Superuser, actor),
%                       {ok, Group} = chef_authz:create_resource(Actor, group),
%                       true = is_authz_id(Group)
                       ok
               end}
      end
     ]}.

