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

-module(pushy_api_util).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").
-include("pushy_event.hrl").

-define(RESPONSE_FORMAT, list). % binary for efficiency, list for debugging
-define(ORG, <<"test_org">>).
-define(NODE, <<"node1">>).
-define(JOB_ID, <<"test_job_id">>).
-define(COMMAND, <<"test_command">>).
-define(INCARNATION, <<"incarnation">>).

mecked() -> [pushy_object, chef_authn, pushy_principal, pushy_wm_base, pushy_key_manager, pushy_job_monitor, pushy_messaging].

applications() -> [folsom, compiler, syntax_tools, goldrush, lager, inets, mochiweb, webmachine, pooler, public_key, ssl, epgsql, sqerl, gproc, jiffy, ibrowse, erlzmq, pushy].

configs() -> [
                {pushy, [
                  {api_port, 10003}
                , {log_dir, "/tmp"}
                , {heartbeat_interval, 1000}
                , {detect_offline_nodes_interval, 4}
                , {zeromq_listen_address, "tcp://*" }
                , {server_heartbeat_port, 10000}
                , {command_port, 10002}
                , {chef_api_version, "11.0.0"}
                , {erchef_root_url, "https://localhost"}
                , {server_name, "localhost"}
                , {skip_header_validation, true}
               ]}
              , {pushy_common, [
                  {enable_graphite, false}
               ]}
              , {sqerl, [
                  {db_driver_mod, sqerl_pgsql_client}
                , {db_host, "127.0.0.1"}
                , {db_port, 5432}
                , {db_user, "opscode_pushy"}
                , {db_pass, "de58233901fcbc3512979571dae6abf85e175ad108062eda5cf307b1f4706f2e5537524c35528f654bfeac183fd45c7a8408"}
                , {db_name,   "opscode_pushy" }
                , {idle_check, 10000}
                , {prepared_statements, {pushy_sql, statements, []} }
                , {column_transforms, []}
               ]}
              , {pooler, [
                  {pools, [[{name, sqerl},
                            {max_count, 20},
                            {init_count, 20 },
                            {start_mfa, {sqerl_client, start_link, []}}]]}
                , {metrics_module, folsom_metrics}
               ]}
              , {chef_authn, [
                  {keyring_dir, "../test"}
                , {keyring, [  {pivotal, "../test/testkey.pem"}
                             , {pushy_priv, "../test/testkey.pem"}]}
               ]}
             ].

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

get_addr(NodeRef) -> iolist_to_binary(io_lib:format("~p-addr", [NodeRef])).

start_node() -> start_node(make_node_ref(?ORG, ?NODE)).

start_node(NodeRef) ->
    {ok, NodePid} = pushy_node_state:start_link(NodeRef, get_addr(NodeRef), ?INCARNATION),
    erlang:unlink(NodePid), % test should ignore when pid does
    pushy_node_state:aborted(NodeRef),  % "respond" to the abort message we know is sent
    NodePid.

send_msg(NodeRef, NodePid, JobId, Type) ->
    TBin = list_to_binary(atom_to_list(Type)),
    Msg = [{<<"job_id">>, JobId},
           {<<"type">>, TBin},
           {<<"timestamp">>, list_to_binary(httpd_util:rfc1123_date())}],
    NodePid ! {raw_message, [get_addr(NodeRef), <<"Version:2.0;SigningMethod:hmac_sha256">>, jiffy:encode({Msg})]}.

run_job(NodePid, JobId) -> run_job(NodePid, JobId, make_node_ref(?ORG, ?NODE)).

run_job(NodePid, JobId, NodeRef) ->
    {OrgName, Node} = NodeRef,
    JobNodes = [#pushy_job_node{org_id=OrgName, node_name=Node, status=new}],
    {ok, JobPid} = pushy_job_state:start_link(#pushy_job{id=JobId, job_nodes=JobNodes, command=?COMMAND, quorum=1, run_timeout=1}, requestor),
    erlang:unlink(JobPid),
    % "respond" to the commit messsage we know is sent
    send_msg(NodeRef, NodePid, JobId, ack_commit),
    % "respond" to the run messsage we know is sent
    send_msg(NodeRef, NodePid, JobId, ack_run),
    % claim to have succeeded at running the command
    timer:apply_after(500, ?MODULE, send_msg, [NodeRef, NodePid, JobId, succeeded]),
    JobPid.

get_feed(CreatorName, OrgName, Path, AsyncReceiver, LastEventId) when is_binary(LastEventId) ->
    get_feed(CreatorName, OrgName, Path, AsyncReceiver, binary_to_list(LastEventId));
get_feed(CreatorName, OrgName, Path, AsyncReceiver, LastEventId) ->
    FullPath = iolist_to_binary(io_lib:format("/organizations/~s/pushy/~s", [OrgName, Path])),
    {ok, PrivateKey} = chef_keyring:get_key(pushy_priv),
    EmptyBody = <<"">>,
    Headers = chef_authn:sign_request(PrivateKey, EmptyBody, CreatorName, <<"GET">>, now, FullPath),
    LEHeader = case LastEventId of
                   no_last_event_id -> [];
                   _ -> [{"Last-Event-ID", LastEventId}]
               end,
    FullHeaders = LEHeader ++ [{"Accept", "text/event-stream"} | Headers],
    Url = "http://localhost:10003" ++ binary_to_list(FullPath),
    case AsyncReceiver of
        synchronous -> sync_http_request(get, Url, FullHeaders, EmptyBody);
        _ -> async_http_request(get, Url, FullHeaders, EmptyBody, AsyncReceiver)
    end.

get_job_feed(AsyncReceiver) ->
    Path = io_lib:format("job_status_feed/~s", [?JOB_ID]),
    % Currently no job tests that use LastEventId
    get_feed(<<"creator_name">>, ?ORG, Path, AsyncReceiver, no_last_event_id).

get_org_feed(LastEventId) -> get_org_feed(LastEventId, ?ORG).

get_org_feed(LastEventId, OrgName) ->
    % note that, unlike get_job_feed, we don't allow a synchronous request, since it would never
    % finish (since the org feed never closes the connection)
    get_feed(<<"creator_name">>, OrgName, "job_status_feed", self(), LastEventId).

get_job_subscribers(JobId) ->
    case pushy_job_state_sup:get_process(JobId) of
        not_found -> not_found;
        Pid -> 
            {ok, Subs} = pushy_fsm_utils:safe_sync_send_all_state_event(Pid, get_subscribers),
            % Filter out the pushy_org_events subs
            lists:filter(fun(P) ->
                                 {dictionary, D} = erlang:process_info(P, dictionary),
                                 case lists:keyfind('$initial_call', 1, D) of
                                     {_, {pushy_org_events,_,_}} -> false;
                                     _ -> true
                                 end
                         end, Subs)
    end.
    
sync_http_request(Method, Url, Headers, Body) ->
    Res = ibrowse:send_req(Url, Headers, Method, Body, [{response_format, ?RESPONSE_FORMAT}]),
    case Res of
        {ok, Code, ResHeaders, ResBody} -> {ok, Code, ResHeaders, list_to_binary(ResBody)};
        _ -> Res
    end.

async_http_request(Method, Url, Headers, Body, Receiver) ->
    % _have_ to use {stream_to, {Pid, once}} if we don't the data to be accumulated in chunks of some byte-size, but
    % want to handle the data as it comes in.
    % Since we're turning on active-once, we have to remember to reset the "active-once" flag after every chunk of data
    {ibrowse_req_id, ReqId} = ibrowse:send_req(Url, Headers, Method, Body, [{stream_to, {Receiver, once}}, {response_format, ?RESPONSE_FORMAT}]),
    ReqId.

receive_async_headers(ReqId) ->
    Res = receive
        {ibrowse_async_headers, ReqId, C, Headers} -> {C, Headers}
    end,
    ibrowse:stream_next(ReqId),
    Res.

make_node_ref(OrgName, NodeName) -> {OrgName, NodeName}.

start_node_and_job() ->
    NodePid = start_node(),
    JobPid = run_job(NodePid, ?JOB_ID),
    {NodePid, JobPid}.

stop_node_and_job(NodePid) ->
    stop_job(?JOB_ID),
    stop_node(NodePid).

stop_job(JobId) ->
    pushy_job_state:stop_job(JobId),
    wait_for_gproc_gone({n, l, {pushy_job, JobId}}).

stop_node(NodePid) -> stop_node(NodePid, make_node_ref(?ORG, ?NODE)).

stop_node(NodePid, NodeRef) ->
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    NodePid ! should_die,
    wait_for_gproc_gone({n,l,GprocName}).

% Drat -- I'd prefer to use gproc:monitor, rather than polling, but that was introduced in a later versino of gproc.
wait_for_gproc_gone(P) ->
    case gproc:where(P) of
        undefined -> ok;
        _ -> timer:sleep(50), wait_for_gproc_gone(P)
    end.

%=================
% SSE event parser
%=================
scan_for_pat(Pat, Size, Bin, Pos) ->
    case Bin of
        <<Pat:Size/binary, _/binary>> -> {yes, Pos};
        <<_, Rest/binary>> -> scan_for_pat(Pat, Size, Rest, Pos+1);
        <<>> -> no
    end.

scan_for(Pat, Bin) ->
    Size = size(Pat),
    case scan_for_pat(Pat, Size, Bin, 0) of
        {yes, Pos} ->
            {Start, <<_:Size/binary, Rest/binary>>} = split_binary(Bin, Pos),
            {yes, Start, Rest};
        no -> no
    end.
    
scan_lflf(Bin) -> scan_for(<<$\n, $\n>>, Bin).
scan_lf(Bin) -> scan_for(<<$\n>>, Bin).
scan_colon(Bin) -> scan_for(<<$:>>, Bin).

split_on(Fun, Bin) -> split_on(Fun, Bin, []).

split_on(Fun, Bin, Acc) -> 
    case Fun(Bin) of
        {yes, Start, Rest} ->
            split_on(Fun, Rest, [Start | Acc]);
        no ->
            {lists:reverse(Acc), Bin}
    end.

split_event(Bin) -> split_on(fun scan_lf/1, Bin).

skip_spaces(<<$ , Rest/binary>>) -> skip_spaces(Rest);
skip_spaces(Rest) -> Rest.

parse_field_val(Bin) ->
    case scan_colon(Bin) of
        {yes, Start, Rest} -> {Start, skip_spaces(Rest)}
    end.

parse_event(Bin) -> 
    {Lines1, Rest} = split_event(Bin),
    Lines = Lines1 ++ [Rest],
    KVs = [parse_field_val(Line) || Line <- Lines],
    lists:foldl(fun update_event/2, #event{}, KVs).

update_event({<<"event">>,V}, E) -> E#event{name=list_to_atom(binary_to_list(V))};
update_event({<<"id">>,V}, E) -> E#event{id=V};
update_event({<<"data">>,V}, E) -> 
    % Assume the JSON is a single object; we'll store its kv list directly
    {struct, PL} = mochijson:decode(V),
    Timestamp = case lists:keyfind("timestamp", 1, PL) of
        {_, Val} -> parse_timestamp(Val);
        false ->
            ?debugVal({no_timestamp, V}),
            ?assert(false)
    end,
    E#event{timestamp = Timestamp, data=PL}.

split_events(Bin) -> split_on(fun scan_lflf/1, Bin).

parse_more_events(Prev, Bin, Final) -> 
    All = <<Prev/binary, Bin/binary>>,
    {EvStrs, Rest} = split_events(All),
    Evs = [parse_event(EvStr) || EvStr <- EvStrs],
    case {Final, Rest} of
        {false, _} -> {Evs, Rest};
        {true, <<>>} -> {Evs, <<>>};
        {true, _} -> {Evs ++ [parse_event(Rest)], <<>>}
    end.

parse_events(Bin, Final) ->
    parse_more_events(<<>>, Bin, Final).
%=================

% XXX Too simple-minded == should parse full ISO-8691 format
parse_timestamp(S) ->
    % "2014-07-30 00:26:48.419302Z"
    case io_lib:fread("~4d-~2d-~2d ~2d:~2d:~2d.~6dZ", S) of
        {ok, [Year, Month, Day, Hour, Minute, Second, USec], _} ->
            {{Year, Month, Day}, {Hour, Minute, Second}, USec}
    end.

get_val(Ev, Key) when is_atom(Key) -> get_val(Ev, atom_to_list(Key));
get_val(Ev, Key) when is_list(Key) ->
    % Don't use proplists:get_value, since we _want_ an exception if it doesn't exist
    case lists:keyfind(Key, 1, Ev#event.data) of
        {Key, Val} -> Val;
        false ->
            ?debugVal({no_such_key, Key, Ev}),
            ?assert(false)
    end.

validate_events(NumEvents, Evs) ->
    ?assertEqual(NumEvents, length(Evs)),
    % Events have unique ids
    ?assertEqual(NumEvents, length(lists:usort([E#event.id || E <- Evs]))),
    % Events have unique timestamps, and are in order
    Ts = [E#event.timestamp || E <- Evs],
    ?assertEqual(Ts, lists:usort(Ts)),
    ok.

expect_start_of_history(Ev) ->
    ?assertEqual(start_of_history, Ev#event.name),
    ExpirationTime = pushy_org_events:get_expiration(),
    NTS = {_, _, NUS} = os:timestamp(),
    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(NTS)) * 1000000 + NUS,
    {ED, ET, EUS} = Ev#event.timestamp,
    Then = calendar:datetime_to_gregorian_seconds({ED, ET}) * 1000000 + EUS,
    ?assert(Now - Then > ExpirationTime * 1000000).

expect_org_job_start(Ev, JobId) ->
    ?assertEqual(start, Ev#event.name),
    ?assertEqual(binary_to_list(JobId), get_val(Ev, job)),
    ?assertEqual(binary_to_list(?COMMAND), get_val(Ev, command)),
    ?assertEqual(1, get_val(Ev, run_timeout)),
    ?assertEqual(1, get_val(Ev, quorum)),
    ?assertEqual(1, get_val(Ev, node_count)),
    ?assertEqual("requestor", get_val(Ev, user)).

expect_org_job_complete(Ev, JobId, Status) ->
    ?assertEqual(job_complete, Ev#event.name),
    ?assertEqual(binary_to_list(JobId), get_val(Ev, job)),
    ?assertEqual(Status, get_val(Ev, status)).

expect_valid_response(NumEvents, Resp) ->
    {Evs, _Rest} = parse_events(Resp, true),
    %?debugVal(Evs),
    validate_events(NumEvents, Evs),
    Evs.

receive_responses(ReqId) ->
    receive
        {ibrowse_async_response, ReqId, _B} ->
            ibrowse:stream_next(ReqId),
            receive_responses(ReqId);
        {ibrowse_async_response_end, ReqId} ->
            ok;
        Other -> ?debugVal(Other)
    end.

receive_next_sse(ReqId) -> receive_next_sse(ReqId, <<>>).

receive_next_sse(ReqId, Prev) ->
    receive
        {ibrowse_async_response, ReqId, B} ->
            Res = parse_more_events(Prev, list_to_binary(B), false),
            ibrowse:stream_next(ReqId),
            Res;
        {ibrowse_async_response_end, ReqId} ->
            {[], Prev}
    end.

receive_sse_so_far(ReqId) -> 
    {Evs, <<>>} = receive_sse_so_far(ReqId, <<>>, []),
    Evs.

receive_sse_so_far(ReqId, Prev, PastEvs) ->
    receive
        {ibrowse_async_response, ReqId, B} ->
            {Evs, Rest} = parse_more_events(Prev, list_to_binary(B), false),
            %?debugVal(Evs),
            ibrowse:stream_next(ReqId),
            receive_sse_so_far(ReqId, Rest, PastEvs ++ Evs);
        {ibrowse_async_response_end, ReqId} ->
            {PastEvs, Prev};
        {ibrowse_async_response, ReqId, Other} ->
            exit({unexpected_ibrowse_msg, {ReqId, Other}});
        {ibrowse_async_headers, ReqId, C, Headers} ->
            exit({unexpected_ibrowse_headers, {ReqId, C, Headers}})
        % Don't catch everything, because non-ReqId messages may be coming in
    after 
        50 ->
            {PastEvs, Prev}
    end.

t(Title, Fun) ->
    {Title, fun() -> ?debugMsg(Title), Fun() end}.

t1(Title, Fun1) ->
    % Despite the eunit documentation, we can't include "Title" at the beginning of the tuple
    {with, [fun(V) -> ?debugMsg(Title), Fun1(V) end]}.

% Note -- these tests are not comprehensive -- as it stands there are plenty of RSpec-based tests that verify the
% behavior.  If we ever migrate away from RSpec, then those tests should be re-written here.
one_node_tests() -> {
    foreach,
    fun start_node_and_job/0,
    fun({NodePid, _}) -> stop_node_and_job(NodePid) end,
    [
     t("Making sure a synchronous Server-side-event request works", fun sync_works/0),
     t("Making sure an asynchronous Server-side-event request works", fun async_works/0),
     t1("On early close, the job stops trying to send to the (exited) subscriber", fun early_close/1)
    ]
}.

org_tests() -> {
    foreach,
    fun start_node/0,
    fun stop_node/1,
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

% I like "focus" from RSpec.  This implements the same functionality -- just surround a test with {focus,...}.  All such
% tests are run.  Note that you can't do any entire group; just individual tests.  And this only works if argument is
% a list that looks like [{_, _, _, [TestCases]}].
filter_tests(TL) ->
    % Split how-to-run from the lists of tests cases
    SplitTs = [{TCs, {T1,T2,T3}} || {T1,T2,T3,TCs} <- TL],
    % Filter the test-case-lists to those that specify {focus,...}
    SplitFoci = lists:map(fun({TCs, THow}) ->
                                  FocusedTCs = [TC || {focus, TC} <- TCs],
                                  {FocusedTCs, THow}
                          end, SplitTs),
    % If there are any focuses, build those back to the original form, otherwise just return the original list.
    case lists:any(fun({FocusedTCs, _}) -> FocusedTCs /= [] end, SplitFoci) of
        true -> [{T1,T2,T3,TCs} || {TCs,{T1,T2,T3}} <- SplitFoci];
        _ -> TL
    end.

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
