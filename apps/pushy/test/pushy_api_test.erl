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
                         trace_module/1,
                         configs/0,
                         start_node_and_job/0,
                         stop_node_and_job/2,
                         start_node/0,
                         start_node/1,
                         stop_node/1,
                         stop_node/2,
                         filter_tests/1,
                         get_job_feed/2,
                         expect_valid_response/2,
                         receive_async_headers/1,
                         receive_sse_so_far/1,
                         validate_events/2,
                         validate_event_content/1,
                         get_job_subscribers/1,
                         receive_next_sse/1,
                         stop_job/1,
                         get_org_feed/1,
                         get_org_feed/2,
                         run_default_job/1,
                         run_default_job_with_ref/2,
                         run_default_job_like_this/4,
                         expect_org_job_start/2,
                         expect_org_job_complete/3,
                         expect_start_of_history/1,
                         make_node_ref/2,
                         get_job/2,
                         get_org_jobs/1,
                         post_job/3,
                         send_msg/3,
                         binary_str/2,
                         get_ev_val/2,
                         no_ev_val/2
                        ]).

-define(RESPONSE_FORMAT, list). % binary for efficiency, list for debugging
-define(ORG, <<"test_org">>).
-define(ORGID, <<"123456789012345678901234test_org">>).
-define(ORG2, <<"testorg2">>).
-define(ORG2ID, <<"123456789012345678901234testorg2">>).
-define(NODE, <<"node1">>).
-define(COMMAND, <<"cmd">>).
-define(INCARNATION, <<"inc">>).

mecked() -> [pushy_object, chef_authn, pushy_principal, pushy_wm_base, pushy_key_manager, pushy_job_monitor, pushy_messaging, pushy_node_state].

applications() -> [folsom, compiler, syntax_tools, goldrush, lager, inets, mochiweb, webmachine, pooler, public_key, ssl, epgsql, sqerl, gproc, jiffy, ibrowse, erlzmq, pushy].

basic_setup() ->
    meck:new(mecked(), [passthrough]),
    %meck:expect(pushy_object, create_object,
                %fun(_, _, _) -> {ok, foo} end),
    %meck:expect(pushy_object, update_object,
                %fun(_, _) -> {ok, 1} end),
    %meck:expect(pushy_object, update_object,
                %fun(_, _, _) -> {ok, 1} end),
    meck:expect(pushy_object, fetch_org_id,
                fun(?ORG) -> ?ORGID; (?ORG2) -> ?ORG2ID end),
                %fun(_) -> <<"ABCDEFGH12345678ABCDEFGH12345678">> end),
    meck:expect(chef_authn, authenticate_user_request,
                fun(_, _, _, _, _, _) -> {name, ok} end),
    meck:expect(pushy_principal, fetch_principal,
                fun(_, _) -> #pushy_principal{requestor_key= <<"rk">>, requestor_type=user, requestor_id= <<"pivotal">>} end),
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
    fun({NodePid, JobId, _}) -> stop_node_and_job(NodePid, JobId) end,
    [
     t1("Making sure a synchronous Server-side-event request works", fun sync_works/1),
     t1("Making sure an asynchronous Server-side-event request works", fun async_works/1),
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

param_tests() -> {
  foreach,
  fun() -> start_node() end,
  fun(V) -> stop_node(V) end,
  [
   t("Optional params get passed in to job", fun param_opt/0),
   t("Optional params are handled correctly when they are missing", fun param_missing/0),
   t("Bad optional param values are rejected with a 400", fun param_badenv/0),
   t("Params are not in run msg", fun param_notinrun/0),
   t("Optional file param gets passed in to job", fun param_file/0),
   t("File param can be specified as base64", fun param_file_b64/0),
   t("When no options are specified, no DB row is created", fun db_param_no_opts/0),
   t("When some options are specified, the correct DB row is created", fun db_param_some_opts/0),
   t("When all options are specified, the correct DB row is created", fun db_param_all_opts/0),
   t("When some options are specified, the create_job->fetch_job round-trip works", fun db_param_fetch_rt/0),
   t("When some options are specified, the create_job->fetch_jobs round-trip works", fun db_param_fetch_jobs_rt/0),
   t("When some options are specified, they are reported in the start event", fun db_param_start_event/0),
   t("When file options is specified, 'file_specified=true' is reported in start event", fun db_param_start_file_event/0),
   t("When options are specified, they are reported in the org start event", fun db_param_org_start_event/0)
  ]
 }.

capture_tests() -> {
  foreach,
  fun() -> start_node() end,
  fun(V) -> stop_node(V) end,
  [
   t("Non-boolean capture value is rejected", fun capture_opt_bool/0),
   t("By default, nothing is added to commit messages", fun capture_not_added_to_commit/0),
   t("Capture=true is added to commit messages", fun capture_added_to_commit/0),
   t("Capture=true is added to start event", fun capture_added_to_start_event/0),
   t("Capture=true is added to job resource", fun capture_added_to_job_res/0),
   t("Capture=false is not added to job resource", fun capture_not_added_to_job_res/0),
   t("Capture=false is not added to job resource when dir specified", fun capture_not_added_to_job_res_with_dir/0),
   t("Capture=true is added to org resource", fun capture_added_to_org_res/0),
   t("Capture=false is not added to org resource", fun capture_not_added_to_org_res/0),
   t1("If capture, output blobs from run=succeeded are added to database", fun capture_succeeded_blobs_in_db/1),
   t1("If capture, output blobs from run=failed are added to database", fun capture_failed_blobs_in_db/1),
   t1("If capture with no output, no rows are added", fun capture_no_output_no_row/1),
   t1("If capture with empty output, appropriate row is added", fun capture_empty_output_correct_row/1),
   t1("If capture with empty stderr, appropriate row is added", fun capture_empty_err_correct_row/1),
   t("For a non-existent job, capture resource returns a 404", fun capture_res_404_for_bad_job/0),
   t("For a job with no captured data, capture resource returns a 404", fun capture_res_404_for_no_capture/0),
   t1("For a job with captured stdout, we can get the data via the resource", fun capture_res_ok_for_stdout_capture/1),
   t1("For a job with captured stderr, we can get the data via the resource", fun capture_res_ok_for_stderr_capture/1),
   t1("For a job with captured stdout and stderr, we can get the data via the resource", fun capture_res_ok_for_both_capture/1)
  ]
}.

focust(Title, Fun) -> {focus,t(Title, Fun)}.
focust1(Title, Fun1) -> {focus,t1(Title, Fun1)}.

all_tests() -> filter_tests([
    one_node_tests(),
    org_tests(),
    param_tests(),
    capture_tests()
]).

init_test_() ->
    {setup,
     fun basic_setup/0,
     fun(_) -> basic_cleanup() end,
     all_tests()
    }.

% This is actually a test for this test framework, not for the server itself.
sync_works({_, JobId, _})->
    {ok, "200", _Headers, Resp} = get_job_feed(synchronous, JobId),
    expect_valid_response(6, Resp).

% This is actually a test for this test framework, not for the server itself.
async_works({_, JobId, _}) ->
    ReqId = get_job_feed(self(), JobId),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsFoFar = receive_sse_so_far(ReqId),
    validate_events(4, EvsFoFar),
    timer:sleep(750),
    RemainingEvs = receive_sse_so_far(ReqId),
    validate_events(2, RemainingEvs),
    ibrowse:stream_close(ReqId).

early_close({_, JobId, JobPid}) ->
    ReqId = get_job_feed(self(), JobId),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    ?assertNotEqual([], get_job_subscribers(JobId)),
    _ = receive_next_sse(ReqId),
    ibrowse:stream_close(ReqId),
    % The subscribers don't go away when we close the request; they close the next time the
    % job tries to post an event.
    % We'll ask the job to send a ping.
    JobPid ! ping,
    % Give the 'DOWN' message a moment to arrive
    timer:sleep(750),
    ?assertEqual([], get_job_subscribers(JobId)),
    stop_job(JobId).

org_empty() ->
    ReqId = get_org_feed(no_last_event_id),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    ?assertEqual(0, length(EvsSoFar)),
    ibrowse:stream_close(ReqId).

org_one_old_job(NodePid) ->
    {_, JobId} = run_default_job(NodePid),
    timer:sleep(100),
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_events(0, EvsSoFar),
    timer:sleep(750),
MoreEvs = receive_sse_so_far(ReqId),
    validate_events(1, MoreEvs),
    expect_org_job_complete(hd(MoreEvs), JobId, complete),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

org_one_new_job(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    {_, JobId} = run_default_job(NodePid),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_events(1, EvsSoFar),
    expect_org_job_start(hd(EvsSoFar), JobId),
    timer:sleep(750),
    MoreEvs = receive_sse_so_far(ReqId),
    validate_events(1, MoreEvs),
    expect_org_job_complete(hd(MoreEvs), JobId, complete),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

org_last_event_id(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    {_, JobId} = run_default_job(NodePid),
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
    timer:sleep(750),
    MoreEvs = receive_sse_so_far(ReqId),
    % Get with LEI; check that we can see the recent event
    ReqIdLE2 = get_org_feed(Ev1#event.id),
    {"200", _} = receive_async_headers(ReqIdLE2),
    EvsLE2 = receive_sse_so_far(ReqIdLE2),
    ?assertEqual(MoreEvs, EvsLE2),
    ibrowse:stream_close(ReqIdLE2),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

% When the Last-Event-ID is unrecognized, we get an additional event at the beginning of the
% event list, of type 'start_of_history', which has a timestamp that specifies the history horizon.
org_unrecognized_id(NodePid) ->
    pushy_org_events:clear_events(?ORGID),
    % Let the command start and end
    {_, JobId} = run_default_job(NodePid),
    timer:sleep(750),
    ReqId = get_org_feed(<<"no_such_id">>),
    {"200", _} = receive_async_headers(ReqId),
    Evs = receive_sse_so_far(ReqId),
    % Get the full history
    validate_events(3, Evs),
    [EvH, EvS, EvC] = Evs,
    expect_start_of_history(EvH),
    expect_org_job_start(EvS, JobId),
    expect_org_job_complete(EvC, JobId, complete),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

org_events_expire(NodePid) ->
    OldExpiration = application:get_env(pushy, org_feed_expiration),
    application:set_env(pushy, org_feed_expiration, 1),
    % Get the start event
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    {_, JobId} = run_default_job(NodePid),
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
    stop_job(JobId),
    case OldExpiration of
        undefined -> application:unset_env(pushy, org_feed_expiration);
        _ -> application:set_env(pushy, org_feed_expiration, OldExpiration)
    end.

org_multiple_jobs(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _Headers} = receive_async_headers(ReqId),
    % Deliberately start job2 first, so we can make sure our code for splitting by job below works
    {_, JobId2} = run_default_job(NodePid),
    {_, JobId} = run_default_job(NodePid),
    timer:sleep(750),
    Evs = receive_sse_so_far(ReqId),
    ?assertEqual(4, length(Evs)),
    % No guaranteed order for events across jobs, but within a job, they are guaranteed

    EvsJ1 = [E || E <- Evs, get_ev_val(E, job) == JobId],
    validate_events(2, EvsJ1),
    [EvJ1S, EvJ1C] = EvsJ1,
    expect_org_job_start(EvJ1S, JobId),
    expect_org_job_complete(EvJ1C, JobId, complete),

    EvsJ2 = [E || E <- Evs, get_ev_val(E, job) == JobId2],
    validate_events(2, EvsJ2),
    [EvJ2S, EvJ2C] = EvsJ2,
    expect_org_job_start(EvJ2S, JobId2),
    expect_org_job_complete(EvJ2C, JobId2, complete),

    ibrowse:stream_close(ReqId),
    stop_job(JobId2),
    stop_job(JobId).

org_multiple_orgs(NodePid) ->
    ReqId = get_org_feed(no_last_event_id),
    {"200", _} = receive_async_headers(ReqId),
    {_, JobId} = run_default_job(NodePid),

    Node2 = <<"n2">>,
    Node2Ref = make_node_ref(?ORG2ID, Node2),
    Node2Pid = start_node(Node2Ref),
    ReqId2 = get_org_feed(no_last_event_id, ?ORG2),
    {"200", _} = receive_async_headers(ReqId2),
    ?assertEqual([], receive_sse_so_far(ReqId2)),
    {_JobPid2, JobId2} = run_default_job_with_ref(Node2Pid, Node2Ref),

    timer:sleep(750),

    Evs1 = receive_sse_so_far(ReqId),
    validate_events(2, Evs1),
    [Ev1S, Ev1C] = Evs1,
    expect_org_job_start(Ev1S, JobId),
    expect_org_job_complete(Ev1C, JobId, complete),

    Evs2 = receive_sse_so_far(ReqId2),
    validate_events(2, Evs2),
    [Ev2S, Ev2C] = Evs2,
    expect_org_job_start(Ev2S, JobId2),
    expect_org_job_complete(Ev2C, JobId2, complete),

    ibrowse:stream_close(ReqId),
    ibrowse:stream_close(ReqId2),

    stop_job(JobId2),
    stop_job(JobId),
    stop_node(Node2Pid, Node2Ref).

% Avoids "not used" error about trace_module
trace(M) -> trace_module(M).

get_sent_msgs() ->
    H = meck:history(pushy_node_state),
    meck:reset(pushy_node_state),
    [Msg || {_, {_,send_msg,[_NodeRefs, {Msg}]},_} <- H].

param_opt() ->
    meck:reset(pushy_node_state),
    Env = [{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}],
    User = <<"user">>,
    Dir = <<"dir">>,
    Params = [{user, User},
              {dir, Dir},
              {env, {Env}}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    [CommitMsg] = get_sent_msgs(),
    ?assertEqual({user, User}, lists:keyfind(user, 1, CommitMsg)),
    ?assertEqual({dir, Dir}, lists:keyfind(dir, 1, CommitMsg)),
    % XX may be too restrictive -- env should be able to be returned in any order
    ?assertEqual({env, {Env}}, lists:keyfind(env, 1, CommitMsg)),
    stop_job(JobId).

param_missing() ->
    meck:reset(pushy_node_state),
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    [CommitMsg] = get_sent_msgs(),
    ?assertEqual(false, lists:keyfind(user, 1, CommitMsg)),
    ?assertEqual(false, lists:keyfind(dir, 1, CommitMsg)),
    ?assertEqual(false, lists:keyfind(env, 1, CommitMsg)),
    ?assertEqual(false, lists:keyfind(file, 1, CommitMsg)),
    stop_job(JobId).

param_badenv() ->
    meck:reset(pushy_node_state),
    Params = [{job_badarg, <<"foo">>}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params)),
    Params2 = [{user, 123}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params2)),
    Params3 = [{env, "string"}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params3)),
    % Data in env value should have different keys
    Params6 = [{env, {[{<<"foo">>, <<"bleah">>}, {<<"foo">>, <<"bar">>}]}}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params6)),
    Params7 = [{file, <<"noprefix">>}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params7)),
    Params8 = [{file, <<"base64:bad$base64">>}],
    ?assertMatch({error, "400", _}, post_job(?COMMAND, [?NODE], Params8)).

param_notinrun() ->
    Env = [{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}],
    User = <<"user">>,
    Dir = <<"dir">>,
    Params = [{user, User},
              {dir, Dir},
              {env, {Env}},
              {file, <<"raw:foo">>}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    meck:reset(pushy_node_state),
    % "respond" to the commit messsage we know is sent
    NodeRef = make_node_ref(?ORGID, ?NODE),
    send_msg(NodeRef, JobId, ack_commit),
    timer:sleep(100),
    [RunMsg] = get_sent_msgs(),
    ?assertEqual(false, lists:keyfind(user, 1, RunMsg)),
    ?assertEqual(false, lists:keyfind(dir, 1, RunMsg)),
    ?assertEqual(false, lists:keyfind(env, 1, RunMsg)),
    ?assertEqual(false, lists:keyfind(file, 1, RunMsg)),
    stop_job(JobId).

param_file() ->
    meck:reset(pushy_node_state),
    FileStr = <<"raw:test">>,
    Params = [{file, FileStr}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    [CommitMsg] = get_sent_msgs(),
    {file, FileMsg} = lists:keyfind(file, 1, CommitMsg),
    ?assertEqual(FileStr, FileMsg),
    stop_job(JobId).

param_file_b64() ->
    meck:reset(pushy_node_state),
    FileBase = <<"test">>,
    B64 = base64:encode(FileBase),
    FileStr = <<"base64:", B64/binary>>,
    Params = [{file, FileStr}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    [CommitMsg] = get_sent_msgs(),
    ?assertEqual({file, FileStr}, lists:keyfind(file, 1, CommitMsg)),
    stop_job(JobId).

db_param_no_opts() ->
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    {ok, Rows} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    ?assertEqual([], Rows),
    stop_job(JobId).

db_param_some_opts() ->
    User = <<"user">>,
    Dir = <<"dir">>,
    Params = [{user, User}, {dir, Dir}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {ok, Rows} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    ?assertEqual(1, length(Rows)),
    [Row] = Rows,
    {_, DbUser} = lists:keyfind(<<"job_user">>, 1, Row),
    % We use binary_str when we just want to check the prefix, since the schema
    % might mean the column is fixed-width.
    ?assertEqual(1, binary_str(DbUser, User)),
    {_, DbDir} = lists:keyfind(<<"dir">>, 1, Row),
    ?assertEqual(1, binary_str(DbDir, Dir)),
    {_, DbEnv} = lists:keyfind(<<"env">>, 1, Row),
    ?assertEqual(null, DbEnv),
    {_, DbFile} = lists:keyfind(<<"job_file">>, 1, Row),
    ?assertEqual(null, DbFile),
    stop_job(JobId).

db_param_all_opts() ->
    User = <<"user">>,
    Dir = <<"dir">>,
    Env = {[{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}]},
    File = <<"raw:foo">>,
    Params = [{user, User},
              {dir, Dir},
              {env, Env},
              {file, File}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {ok, Rows} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    ?assertEqual(1, length(Rows)),
    [Row] = Rows,
    {_, DbUser} = lists:keyfind(<<"job_user">>, 1, Row),
    % We use binary_str when we just want to check the prefix, since the schema
    % might mean the column is fixed-width.
    ?assertEqual(1, binary_str(DbUser, User)),
    {_, DbDir} = lists:keyfind(<<"dir">>, 1, Row),
    ?assertEqual(1, binary_str(DbDir, Dir)),
    {_, DbEnv} = lists:keyfind(<<"env">>, 1, Row),
    ?assertEqual(jiffy:encode(Env), DbEnv),
    {_, DbFile} = lists:keyfind(<<"job_file">>, 1, Row),
    ?assertEqual(File, DbFile),
    stop_job(JobId).

% That's enough tests of the low-level representation.  Now let's just make sure that
% round-trips work.

db_param_fetch_rt() ->
    User = <<"user">>,
    Dir = <<"dir">>,
    Params = [{user, User}, {dir, Dir}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {ok, DbJob} = pushy_sql:fetch_job(JobId),
    #pushy_job{opts = #pushy_job_opts{user = DbUser, dir = DbDir, env = DbEnv, file = DbFile}} = DbJob,
    ?assertEqual(User, DbUser),
    ?assertEqual(Dir, DbDir),
    ?assertEqual(undefined, DbEnv),
    ?assertEqual(undefined, DbFile),
    stop_job(JobId).

db_param_fetch_jobs_rt() ->
    Env = {[{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}]},
    File = <<"raw:foo">>,
    Params = [{env, Env}, {file, File}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {ok, DbOrgJobs} = pushy_sql:fetch_jobs(?ORGID),
    [DbJob] = [J || J <- DbOrgJobs, J#pushy_job.id =:= JobId],
    #pushy_job{opts = #pushy_job_opts{user = DbUser, dir = DbDir, env = DbEnv, file = DbFile}} = DbJob,
    ?assertEqual(undefined, DbUser),
    ?assertEqual(undefined, DbDir),
    ?assertEqual(Env, DbEnv),
    ?assertEqual(File, DbFile),
    stop_job(JobId).

db_param_start_event() ->
    User = <<"user">>,
    Env = {[{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}]},
    Params = [{user, User}, {env, Env}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    ReqId = get_job_feed(self(), JobId),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_event_content(EvsSoFar),
    Ev = hd(EvsSoFar),
    ?assertEqual(start, Ev#event.name),
    ?assertEqual(User, get_ev_val(Ev, job_user)),
    ?assert(no_ev_val(Ev, dir)),
    ?assertEqual(Env, get_ev_val(Ev, env)),
    ?assert(no_ev_val(Ev, file_specified)),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

db_param_start_file_event() ->
    File = <<"raw:foo">>,
    Params = [{file, File}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    ReqId = get_job_feed(self(), JobId),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_event_content(EvsSoFar),
    Ev = hd(EvsSoFar),
    % "file" _isn't_ included
    ?assert(no_ev_val(Ev, file)),
    % "file_specified" _is_ included
    ?assertEqual(true, get_ev_val(Ev, file_specified)),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

db_param_org_start_event() ->
    ReqId = get_org_feed(no_last_event_id),
    Dir = <<"dir">>,
    Env = {[{<<"ANOTHER_ENV">>, <<"another_val">>}, {<<"ONE_MORE">>, <<"more">>}]},
    File = <<"raw:foo">>,
    Params = [{dir, Dir}, {env, Env}, {file, File}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_event_content(EvsSoFar),
    Ev = hd(EvsSoFar),
    ?assertEqual(start, Ev#event.name),
    ?assert(no_ev_val(Ev, job_user)),
    ?assertEqual(Dir, get_ev_val(Ev, dir)),
    ?assertEqual(Env, get_ev_val(Ev, env)),
    ?assertEqual(true, get_ev_val(Ev, file_specified)),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

capture_opt_bool() ->
    Params = [{<<"capture_output">>, 1}],
    {error, Code, _} = post_job(?COMMAND, [?NODE], Params),
    ?assertEqual("400", Code).

capture_not_added_to_commit() ->
    meck:reset(pushy_node_state),
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    [CommitMsg] = get_sent_msgs(),
    ?assertEqual(undefined, proplists:get_value(file, CommitMsg)),
    stop_job(JobId).

capture_added_to_commit() ->
    Params = [{<<"capture_output">>, true}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    [CommitMsg] = get_sent_msgs(),
    ?assertEqual(true, proplists:get_value(capture, CommitMsg)),
    stop_job(JobId).

capture_added_to_start_event() ->
    Params = [{capture_output, true}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    ReqId = get_job_feed(self(), JobId),
    {Code, _Headers} = receive_async_headers(ReqId),
    ?assertEqual("200", Code),
    EvsSoFar = receive_sse_so_far(ReqId),
    validate_event_content(EvsSoFar),
    Ev = hd(EvsSoFar),
    ?assertEqual(start, Ev#event.name),
    ?assertEqual(true, get_ev_val(Ev, capture)),
    ibrowse:stream_close(ReqId),
    stop_job(JobId).

capture_added_to_job_res() ->
    Params = [{capture_output, true}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {Job} = get_job(JobId, ?ORG),
    ?assertEqual(true, ej:get({"capture_output"}, Job)),
    stop_job(JobId).

capture_not_added_to_job_res() ->
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    {Job} = get_job(JobId, ?ORG),
    ?assertEqual(undefined, ej:get({"capture_output"}, Job)),
    stop_job(JobId).

% No rows are written to the DB if there are _no_ options, so let's
% force a row to be written.
capture_not_added_to_job_res_with_dir() ->
    Params = [{dir, <<"/tmp">>}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    {Job} = get_job(JobId, ?ORG),
    ?assertEqual(undefined, ej:get({"capture_output"}, Job)),
    stop_job(JobId).

capture_added_to_org_res() ->
    Params = [{capture_output, true}],
    {ok, JobId} = post_job(?COMMAND, [?NODE], Params),
    Jobs = get_org_jobs(?ORG),
    [Job] = [J || J <- Jobs, ej:get({"id"}, J) =:= JobId],
    ?assertEqual(true, ej:get({"capture_output"}, Job)),
    stop_job(JobId).

capture_not_added_to_org_res() ->
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    Jobs = get_org_jobs(?ORG),
    [Job] = [J || J <- Jobs, ej:get({"id"}, J) =:= JobId],
    ?assertEqual(undefined, ej:get({"capture_output"}, Job)),
    stop_job(JobId).

capture_succeeded_blobs_in_db(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    ResultParams = [{<<"stdout">>, <<"testout">>},
                    {<<"stderr">>, <<"testerr">>}],
    Result = {succeeded, ResultParams},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    {ok, [OptRow]} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    {_, Capture} = lists:keyfind(<<"capture">>, 1, OptRow),
    ?assertEqual(true, Capture),
    {ok, [OutRow]} = sqerl:execute(<<"select * from job_output where job_id = $1">>,
                               [JobId]),
    {_, Out} = lists:keyfind(<<"stdout">>, 1, OutRow),
    ?assertEqual(<<"testout">>, Out),
    {_, Err} = lists:keyfind(<<"stderr">>, 1, OutRow),
    ?assertEqual(<<"testerr">>, Err),
    stop_job(JobId).

capture_failed_blobs_in_db(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    ResultParams = [{<<"stdout">>, <<"testout">>},
                    {<<"stderr">>, <<"testerr">>}],
    Result = {failed, ResultParams},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    {ok, [OptRow]} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    {_, Capture} = lists:keyfind(<<"capture">>, 1, OptRow),
    ?assertEqual(true, Capture),
    {ok, [OutRow]} = sqerl:execute(<<"select * from job_output where job_id = $1">>,
                               [JobId]),
    {_, Out} = lists:keyfind(<<"stdout">>, 1, OutRow),
    ?assertEqual(<<"testout">>, Out),
    {_, Err} = lists:keyfind(<<"stderr">>, 1, OutRow),
    ?assertEqual(<<"testerr">>, Err),
    stop_job(JobId).

capture_no_output_no_row(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    Result = {succeeded, []},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    {ok, [OptRow]} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    {_, Capture} = lists:keyfind(<<"capture">>, 1, OptRow),
    ?assertEqual(true, Capture),
    {ok, []} = sqerl:execute(<<"select * from job_output where job_id = $1">>,
                               [JobId]),
    stop_job(JobId).

capture_empty_output_correct_row(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    % Note we are testing both strings and binaries, just in case
    Result = {succeeded, [{<<"stdout">>, ""}, {<<"stderr">>, <<"">>}]},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    {ok, [OptRow]} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    {_, Capture} = lists:keyfind(<<"capture">>, 1, OptRow),
    ?assertEqual(true, Capture),
    {ok, [OutRow]} = sqerl:execute(<<"select * from job_output where job_id = $1">>,
                               [JobId]),
    {_, Out} = lists:keyfind(<<"stdout">>, 1, OutRow),
    ?assertEqual(<<"">>, Out),
    {_, Err} = lists:keyfind(<<"stderr">>, 1, OutRow),
    ?assertEqual(<<"">>, Err),
    stop_job(JobId).

capture_empty_err_correct_row(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    Result = {failed, [{<<"stdout">>, "testout"}, {<<"stderr">>, <<"">>}]},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    {ok, [OptRow]} = sqerl:execute(<<"select * from job_options where job_id = $1">>,
                               [JobId]),
    {_, Capture} = lists:keyfind(<<"capture">>, 1, OptRow),
    ?assertEqual(true, Capture),
    {ok, [OutRow]} = sqerl:execute(<<"select * from job_output where job_id = $1">>,
                               [JobId]),
    {_, Out} = lists:keyfind(<<"stdout">>, 1, OutRow),
    ?assertEqual(<<"testout">>, Out),
    {_, Err} = lists:keyfind(<<"stderr">>, 1, OutRow),
    ?assertEqual(<<"">>, Err),
    stop_job(JobId).

capture_res_404_for_bad_job() ->
    Path = io_lib:format("jobs/no_such_job/output/~s/stdout", [?NODE]),
    {ok, Code, _Headers, _Resp} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   Path, synchronous, [], <<"">>),
    ?assertEqual("404", Code).

capture_res_404_for_no_capture() ->
    {ok, JobId} = post_job(?COMMAND, [?NODE], []),
    Path = io_lib:format("jobs/~s/output/~s/stdout", [JobId, ?NODE]),
    {ok, Code, _Headers, _Resp} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   Path, synchronous, [], <<"">>),
    ?assertEqual("404", Code).

capture_res_ok_for_stdout_capture(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    ResultParams = [{<<"stdout">>, <<"testout">>}, {<<"stderr">>, <<"">>}],
    Result = {succeeded, ResultParams},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    PathO = io_lib:format("jobs/~s/output/~s/stdout", [JobId, ?NODE]),
    {ok, CodeO, _HeadersO, RespO} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathO, synchronous, [], <<"">>),
    ?assertEqual("200", CodeO),
    ?assertEqual(<<"testout">>, RespO),
    PathE = io_lib:format("jobs/~s/output/~s/stderr", [JobId, ?NODE]),
    {ok, CodeE, _HeadersE, RespE} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathE, synchronous, [], <<"">>),
    ?assertEqual("200", CodeE),
    ?assertEqual(<<"">>, RespE).

capture_res_ok_for_stderr_capture(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    ResultParams = [{<<"stdout">>, <<"">>}, {<<"stderr">>, <<"testerr">>}],
    Result = {succeeded, ResultParams},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    PathO = io_lib:format("jobs/~s/output/~s/stdout", [JobId, ?NODE]),
    {ok, CodeO, _HeadersO, RespO} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathO, synchronous, [], <<"">>),
    ?assertEqual("200", CodeO),
    ?assertEqual(<<"">>, RespO),
    PathE = io_lib:format("jobs/~s/output/~s/stderr", [JobId, ?NODE]),
    {ok, CodeE, _HeadersE, RespE} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathE, synchronous, [], <<"">>),
    ?assertEqual("200", CodeE),
    ?assertEqual(<<"testerr">>, RespE).

capture_res_ok_for_both_capture(NodePid) ->
    meck:reset(pushy_node_state),
    NodeRef = make_node_ref(?ORGID, ?NODE),
    Opts = #pushy_job_opts{capture=true},
    ResultParams = [{<<"stdout">>, <<"testout">>},
                    {<<"stderr">>, <<"testerr">>}],
    Result = {succeeded, ResultParams},
    {_, JobId} = run_default_job_like_this(NodePid, NodeRef, Opts, Result),
    timer:sleep(1000),
    PathO = io_lib:format("jobs/~s/output/~s/stdout", [JobId, ?NODE]),
    {ok, CodeO, _HeadersO, RespO} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathO, synchronous, [], <<"">>),
    ?assertEqual("200", CodeO),
    ?assertEqual(<<"testout">>, RespO),
    PathE = io_lib:format("jobs/~s/output/~s/stderr", [JobId, ?NODE]),
    {ok, CodeE, _HeadersE, RespE} =
        pushy_api_util:api_request(get, <<"creator_name">>, ?ORG, 
                                   PathE, synchronous, [], <<"">>),
    ?assertEqual("200", CodeE),
    ?assertEqual(<<"testerr">>, RespE).
