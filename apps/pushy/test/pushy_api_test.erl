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

-module(pushy_api_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").
-include("pushy_event.hrl").
-import(pushy_api_util, [
                         configs/0,
                         start_node_and_job/0,
                         stop_node_and_job/1,
                         start_node/0,
                         start_node/1,
                         stop_node/1,
                         stop_node/2,
                         filter_tests/1,
                         get_job_feed/1,
                         expect_valid_response/2,
                         receive_async_headers/1,
                         receive_sse_so_far/1,
                         validate_events/2,
                         get_job_subscribers/1,
                         receive_next_sse/1,
                         stop_job/1,
                         get_org_feed/1,
                         get_org_feed/2,
                         run_job/2,
                         run_job/3,
                         expect_org_job_start/2,
                         expect_org_job_complete/3,
                         expect_start_of_history/1,
                         get_val/2,
                         make_node_ref/2
                        ]).

-define(RESPONSE_FORMAT, list). % binary for efficiency, list for debugging
-define(ORG, <<"test_org">>).
-define(NODE, <<"node1">>).
-define(JOB_ID, <<"test_job_id">>).
-define(COMMAND, <<"test_command">>).
-define(INCARNATION, <<"incarnation">>).

mecked() -> [pushy_object, chef_authn, pushy_principal, pushy_wm_base, pushy_key_manager, pushy_job_monitor, pushy_messaging].

applications() -> [folsom, compiler, syntax_tools, goldrush, lager, inets, mochiweb, webmachine, pooler, public_key, ssl, epgsql, sqerl, gproc, jiffy, ibrowse, erlzmq, pushy].

basic_setup() ->
    meck:new(mecked(), [passthrough]),
    meck:expect(pushy_object, create_object,
                fun(_, _, _) -> {ok, foo} end),
    meck:expect(pushy_object, update_object,
                fun(_, _) -> {ok, 1} end),
    meck:expect(pushy_object, update_object,
                fun(_, _, _) -> {ok, 1} end),
    meck:expect(pushy_object, fetch_org_id,
                fun(X) -> X end),
    meck:expect(chef_authn, authenticate_user_request,
                fun(_, _, _, _, _, _) -> {name, ok} end),
    meck:expect(pushy_principal, fetch_principal,
                fun(_, _) -> #pushy_principal{requestor_key= <<"rk">>, requestor_type=user, requestor_id= <<"pivotal">>} end),
    meck:expect(pushy_wm_base, is_authorized,
                fun(Req, State) -> {true, Req, State} end),
    meck:expect(pushy_wm_base, read_forbidden,
                fun(Req, State) -> {false, Req, State} end),
    meck:expect(pushy_wm_base, write_forbidden,
                fun(Req, State) -> {false, Req, State} end),
    meck:expect(pushy_key_manager, get_key,
                fun(_) -> {hmac_sha256, <<"key">>} end),
    meck:expect(pushy_job_monitor, monitor_job,
                fun(_, _) -> ok end),
    meck:expect(pushy_messaging, check_timestamp,
                fun(_, _) -> ok end),
    lists:foreach(fun({A, Cs}) -> [application:set_env(A, P, V) || {P, V} <- Cs] end, configs()),
    lists:foreach(fun(A) -> ok = application:start(A) end, applications()),
    ok.

basic_cleanup() ->
%   application:stop(lager),    % needs to stop to ensure that "rebar eunit" exits quickly
%   % Note -- pooler will generate lots of messages on cleanup as long as it uses "brutal_kill" for some
%   % of the supervsior shutdown strategies.
%   application:stop(pushy),
    [application:stop(A) || A <- lists:reverse(applications())],
    [meck:unload(M) || M <- lists:reverse(mecked())],
    ok.

t(Title, Fun) ->
    {Title, fun() -> ?debugMsg(Title), Fun() end}.

t1(Title, Fun1) ->
    % Despite the eunit documentation, we can't include "Title" at the beginning of the tuple
    {with, [fun(V) -> ?debugMsg(Title), Fun1(V) end]}.

% Note -- these tests are not comprehensive -- as it stands there are plenty of RSpec-based tests that verify the
% behavior.  If we ever migrate away from RSpec, then those tests should be re-written here.
one_node_tests() -> {
    foreach,
    fun() -> start_node_and_job() end,
    fun({NodePid, _}) -> stop_node_and_job(NodePid) end,
    [
     t("Making sure a synchronous Server-side-event request works", fun sync_works/0),
     t("Making sure an asynchronous Server-side-event request works", fun async_works/0),
     t1("On early close, the job stops trying to send to the (exited) subscriber", fun early_close/1)
    ]
}.

org_tests() -> {
    foreach,
    fun() -> start_node() end,
    fun(V) -> stop_node(V) end,
    [
     t("Org request returns an empty list at first", fun org_empty/0),
     t1("Org request returns job stop for already-running job", fun org_one_old_job/1),
     t1("Org request returns job start/stop for new job", fun org_one_new_job/1),
     t1("Org request handles Last-Event-ID", fun org_last_event_id/1),
     t1("Org synthesized start-of-history if Last-Event-ID is unrecognized", fun org_unrecognized_id/1),
     t1("Org events expire over time", fun org_events_expire/1),
     t1("Org events for multiple jobs works", fun org_multiple_jobs/1),
     t1("Org events work across multiple organizations", fun org_multiple_orgs/1)
    ]
}.

focust(Title, Fun) -> {focus,t(Title, Fun)}.
focust1(Title, Fun1) -> {focus,t1(Title, Fun1)}.

all_tests() -> filter_tests([
    one_node_tests(),
    org_tests()
]).

init_test_() ->
    {setup,
     fun basic_setup/0,
     fun(_) -> basic_cleanup() end,
     all_tests()
    }.

% This is actually a test for this test framework, not for the server itself.
sync_works()->
    {ok, "200", _Headers, Resp} = get_job_feed(synchronous),
    expect_valid_response(6, Resp).

% This is actually a test for this test framework, not for the server itself.
async_works() ->
    ReqId = get_job_feed(self()),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsFoFar = receive_sse_so_far(ReqId),
    validate_events(4, EvsFoFar),
    timer:sleep(500),
    RemainingEvs = receive_sse_so_far(ReqId),
    validate_events(2, RemainingEvs),
    ibrowse:stream_close(ReqId).

early_close({_, JobPid}) ->
    ReqId = get_job_feed(self()),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    ?assertNotEqual([], get_job_subscribers(?JOB_ID)),
    _ = receive_next_sse(ReqId),
    ibrowse:stream_close(ReqId),
    % The subscribers don't go away when we close the request; they close the next time the
    % job tries to post an event.
    % We'll ask the job to send a ping.
    JobPid ! ping,
    % Give the 'DOWN' message a moment to arrive
    timer:sleep(500),
    ?assertEqual([], get_job_subscribers(?JOB_ID)),
    stop_job(?JOB_ID).

org_empty() ->
    ReqId = get_org_feed(no_last_event_id),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    ?assertEqual(0, length(EvsSoFar)),
    ibrowse:stream_close(ReqId).

org_one_old_job(NodePid) ->
    run_job(NodePid, ?JOB_ID),
    timer:sleep(100),
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_events(0, EvsSoFar),
    timer:sleep(500),
    MoreEvs = receive_sse_so_far(ReqId),
    validate_events(1, MoreEvs),
    expect_org_job_complete(hd(MoreEvs), ?JOB_ID, "complete"),
    ibrowse:stream_close(ReqId),
    stop_job(?JOB_ID).

org_one_new_job(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    run_job(NodePid, ?JOB_ID),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_events(1, EvsSoFar),
    expect_org_job_start(hd(EvsSoFar), ?JOB_ID),
    timer:sleep(500),
    MoreEvs = receive_sse_so_far(ReqId),
    validate_events(1, MoreEvs),
    expect_org_job_complete(hd(MoreEvs), ?JOB_ID, "complete"),
    ibrowse:stream_close(ReqId),
    stop_job(?JOB_ID).

org_last_event_id(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    run_job(NodePid, ?JOB_ID),
    % Get evs without LastEventID
    EvsSoFar = receive_sse_so_far(ReqId),
    Ev1 = hd(EvsSoFar),
    % Get with LEI -- should be empty
    ReqIdLE1 = get_org_feed(Ev1#event.id),
    {"200", _} = receive_async_headers(ReqIdLE1),
    EvsLE1 = receive_sse_so_far(ReqIdLE1),
    ?assertEqual([], EvsLE1),
    ibrowse:stream_close(ReqIdLE1),
    % Get new evs without LastEventID
    timer:sleep(500),
    MoreEvs = receive_sse_so_far(ReqId),
    % Get with LEI; check that we can see the recent event
    ReqIdLE2 = get_org_feed(Ev1#event.id),
    {"200", _} = receive_async_headers(ReqIdLE2),
    EvsLE2 = receive_sse_so_far(ReqIdLE2),
    ?assertEqual(MoreEvs, EvsLE2),
    ibrowse:stream_close(ReqIdLE2),
    ibrowse:stream_close(ReqId),
    stop_job(?JOB_ID).

% When the Last-Event-ID is unrecognized, we get an additional event at the beginning of the
% event list, of type 'start_of_history', which has a timestamp that specifies the history horizon.
org_unrecognized_id(NodePid) ->
    pushy_org_events:clear_events(?ORG),
    % Let the command start and end
    run_job(NodePid, ?JOB_ID),
    timer:sleep(600),
    ReqId = get_org_feed(<<"no_such_id">>),
    {"200", _} = receive_async_headers(ReqId),
    Evs = receive_sse_so_far(ReqId),
    % Get the full history
    validate_events(3, Evs),
    [EvH, EvS, EvC] = Evs,
    expect_start_of_history(EvH),
    expect_org_job_start(EvS, ?JOB_ID),
    expect_org_job_complete(EvC, ?JOB_ID, "complete"),
    ibrowse:stream_close(ReqId),
    stop_job(?JOB_ID).

org_events_expire(NodePid) ->
    OldExpiration = application:get_env(pushy, org_feed_expiration),
    application:set_env(pushy, org_feed_expiration, 1),
    % Get the start event
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    run_job(NodePid, ?JOB_ID),
    EvsSoFar = receive_sse_so_far(ReqId),
    Ev1 = hd(EvsSoFar),
    ibrowse:stream_close(ReqId),
    % Wait for job to complete, all events to expire
    timer:sleep(2000),
    % Make sure that no events are available
    ReqIdLE = get_org_feed(Ev1#event.id),
    {"200", _} = receive_async_headers(ReqIdLE),
    EvsLE = receive_sse_so_far(ReqIdLE),
    validate_events(1, EvsLE),
    expect_start_of_history(hd(EvsLE)),
    ibrowse:stream_close(ReqIdLE),
    stop_job(?JOB_ID),
    case OldExpiration of
        undefined -> application:unset_env(pushy, org_feed_expiration);
        _ -> application:set_env(pushy, org_feed_expiration, OldExpiration)
    end.

org_multiple_jobs(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    % Deliberately start job2 first, so we can make sure our code for splitting by job below works
    run_job(NodePid, <<"job2">>),
    run_job(NodePid, ?JOB_ID),
    timer:sleep(600),
    Evs = receive_sse_so_far(ReqId),
    ?assertEqual(4, length(Evs)),
    % No guaranteed order for events across jobs, but within a job, they are guaranteed

    EvsJ1 = [E || E <- Evs, get_val(E, job) == binary_to_list(?JOB_ID)],
    validate_events(2, EvsJ1),
    [EvJ1S, EvJ1C] = EvsJ1,
    expect_org_job_start(EvJ1S, ?JOB_ID),
    expect_org_job_complete(EvJ1C, ?JOB_ID, "complete"),

    EvsJ2 = [E || E <- Evs, get_val(E, job) == binary_to_list(<<"job2">>)],
    validate_events(2, EvsJ2),
    [EvJ2S, EvJ2C] = EvsJ2,
    expect_org_job_start(EvJ2S, <<"job2">>),
    expect_org_job_complete(EvJ2C, <<"job2">>, "complete"),

    ibrowse:stream_close(ReqId),
    stop_job(<<"job2">>),
    stop_job(?JOB_ID).

org_multiple_orgs(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    % Deliberately start job2 first, so we can make sure our code for splitting by job below works
    run_job(NodePid, ?JOB_ID),
    timer:sleep(600),

    Org2 = <<"test_org2">>,
    Node2Name = <<"node2">>,
    Job2 = <<"job2">>,
    Node2Ref = make_node_ref(Org2, Node2Name),
    Node2Pid = start_node(Node2Ref),
    ReqId2 = get_org_feed(no_last_event_id, Org2),
    {"200", _} = receive_async_headers(ReqId2),
    run_job(Node2Pid, Job2, Node2Ref),
    timer:sleep(600),

    Evs1 = receive_sse_so_far(ReqId),
    validate_events(2, Evs1),
    [Ev1S, Ev1C] = Evs1,
    expect_org_job_start(Ev1S, ?JOB_ID),
    expect_org_job_complete(Ev1C, ?JOB_ID, "complete"),

    Evs2 = receive_sse_so_far(ReqId2),
    validate_events(2, Evs2),
    [Ev2S, Ev2C] = Evs2,
    expect_org_job_start(Ev2S, Job2),
    expect_org_job_complete(Ev2C, Job2, "complete"),

    ibrowse:stream_close(ReqId),
    ibrowse:stream_close(ReqId2),

    stop_job(<<"job2">>),
    stop_job(?JOB_ID),
    stop_node(Node2Pid, Node2Ref).
