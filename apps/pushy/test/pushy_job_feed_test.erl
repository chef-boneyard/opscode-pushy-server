%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Steven Grady <steven.grady@erlang-solutions.com>
%%
%% @end

%% @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
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

-module(pushy_job_feed_test).

-define(HB_INTERVAL, 100).
-define(DECAY_WINDOW, 4). %% 4 is friendly to base 2 float arith
-define(DOWN_THRESHOLD, 0.25).

-define(ID, "test_job_id").

-include_lib("eunit/include/eunit.hrl").
-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

mecked() -> [pushy_command_switch, pushy_object, wrq, pushy_node_state].

basic_setup() ->
    test_util:start_apps(),

    meck:new(mecked(), []),
    meck:expect(pushy_command_switch, send,
                fun(_Message) -> ok end),
    meck:expect(pushy_object, create_object,
                fun(_, _, _) -> {ok, foo} end),
    meck:expect(pushy_object, update_object,
                fun(_, _) -> {ok, 1} end),
    meck:expect(pushy_object, update_object,
                fun(_, _, _) -> {ok, 1} end),
    meck:expect(pushy_object, fetch_org_id,
                fun(X) -> X end),
    meck:expect(wrq, path_info, fun(_Id, _Req) -> ?ID end),
    meck:expect(wrq, get_req_header, fun(_Header, _Req) -> undefined end),
    meck:expect(pushy_node_state, watch, fun(_) -> ok end),
    meck:expect(pushy_node_state, status, fun(_) -> {online, {available, none}} end),
    meck:expect(pushy_node_state, send_msg, fun(_, _) -> ok end),
    pushy_org_events_sup:start_link(),
    application:set_env(pushy, heartbeat_interval, ?HB_INTERVAL),
    application:set_env(pushy, decay_window, ?DECAY_WINDOW),
    application:set_env(pushy, down_threshold, ?DOWN_THRESHOLD),
    application:set_env(pushy, server_name, "foo"),

    pushy_key_manager:init(),
    pushy_node_stats:init(),
    ok.


basic_cleanup() ->
    pushy_key_manager:stop(),
    pushy_node_stats:stop(),
    [meck:unload(M) || M <- lists:reverse(mecked())].

init_test_() ->
    {foreach,
     fun basic_setup/0,
     fun(_) ->
             basic_cleanup(),
             ok
     end,
     [fun(_) ->
              {"Make sure that when a job dies, the feed gets notified",
               fun() ->
                       JobNodes = [#pushy_job_node{org_id= <<"org">>, node_name= <<"node1">>, status=new}],
                        JobOpts = #pushy_job_opts{},
                        Job = #pushy_job{id= <<"i">>, job_nodes=JobNodes, command= <<"cmd">>,
                                         quorum=1, run_timeout=1, opts = JobOpts},
                       {ok, JobPid} = pushy_job_state:start_link(Job, requestor),
                       erlang:unlink(JobPid),
                       {{stream, {_Evs, Fun}}, _R, _S} = pushy_feed_resource:to_event_stream(req, #config_state{job_pid = JobPid}),
                       timer:send_after(100, too_late),
                       exit(JobPid, test_kill),
                       % Note: webmachine won't work if you return an empty iolist; it _has_ to be an empty binary
                       ?assertEqual({<<>>, done}, Fun()),
                       % Make sure that this happened before the timer
                       receive
                           too_late -> ?assert(false)
                       after
                           0 -> ok
                       end
               end}
      end
     ]}.

