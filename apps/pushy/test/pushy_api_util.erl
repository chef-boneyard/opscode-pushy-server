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

trace_module(Mod) ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Mod, [{'_', [], [{return_trace}]}]).

% XX Needs to match settings in pushy_api_test.erl
-define(RESPONSE_FORMAT, list). % binary for efficiency, list for debugging
-define(ORG, <<"test_org">>).
-define(ORGID, <<"123456789012345678901234test_org">>).
-define(ORG2, <<"testorg2">>).
-define(ORG2ID, <<"123456789012345678901234testorg2">>).
-define(NODE, <<"node1">>).
-define(COMMAND, <<"cmd">>).
-define(INCARNATION, <<"inc">>).
-define(REQUESTOR, <<"requestor">>).

mecked() -> [pushy_object, chef_authn, pushy_principal, pushy_wm_base, pushy_key_manager, pushy_job_monitor, pushy_messaging].

applications() -> [folsom, compiler, syntax_tools, goldrush, lager, inets, mochiweb, webmachine, pooler, public_key, ssl, epgsql, sqerl, gproc, jiffy, ibrowse, erlzmq, pushy].

configs() ->
             [
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
                , {db_user, "pushy_test"}
                , {db_pass, "password"}
                , {db_name,   "opscode_pushy_test" }
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
                  {keyring_dir, "apps/pushy/test"}
                , {keyring, [  {pivotal, "apps/pushy/test/testkey.pem"}
                             , {pushy_priv, "apps/pushy/test/testkey.pem"}]}
               ]}
             ].

get_user() ->
    case os:getenv("PGUSER") of
        false -> os:getenv("USER");
        User -> User
    end.

get_addr(NodeRef) -> iolist_to_binary(io_lib:format("~p-addr", [NodeRef])).

start_node() -> start_node(make_node_ref(?ORGID, ?NODE)).

start_node(NodeRef) ->
    {ok, NodePid} = pushy_node_state:start_link(NodeRef, get_addr(NodeRef), ?INCARNATION),
    erlang:unlink(NodePid), % test should ignore when pid dies
    pushy_node_state:aborted(NodeRef),  % "respond" to the abort message we know is sent
    NodePid.

send_msg(NodeRef, JobId, Type) ->
    TBin = list_to_binary(atom_to_list(Type)),
    Msg = [{<<"job_id">>, JobId},
           {<<"type">>, TBin},
           {<<"timestamp">>, list_to_binary(httpd_util:rfc1123_date())}],
    FullMsg = [get_addr(NodeRef), <<"Version:2.0;SigningMethod:hmac_sha256">>,
               jiffy:encode({Msg})],
    pushy_node_state:recv_msg(FullMsg).

send_msg(NodeRef, NodePid, JobId, Type) ->
    send_msg(NodeRef, NodePid, JobId, Type, []).

send_msg(NodeRef, NodePid, JobId, Type, MoreParams) ->
    TBin = list_to_binary(atom_to_list(Type)),
    Msg = [{<<"job_id">>, JobId},
           {<<"type">>, TBin},
           {<<"timestamp">>, list_to_binary(httpd_util:rfc1123_date())}] ++
           MoreParams,
    NodePid ! {raw_message, [get_addr(NodeRef),
                             <<"Version:2.0;SigningMethod:hmac_sha256">>,
                             jiffy:encode({Msg})]}.

run_default_job(NodePid) ->
    NodeRef = make_node_ref(?ORGID, ?NODE),
    run_default_job_with_ref(NodePid, NodeRef).

run_default_job_with_ref(NodePid, NodeRef) ->
    Opts = #pushy_job_opts{},
    run_default_job_like_this(NodePid, NodeRef, Opts, {succeeded, []}).

run_default_job_like_this(NodePid, NodeRef, Opts, Result) ->
    {OrgId, Node} = NodeRef,
    JobId = pushy_object:make_org_prefix_id(OrgId),
    Now = pushy_sql:sql_date(now),
    JobNodes = [#pushy_job_node{job_id=JobId, org_id=OrgId, node_name=Node,
                                status=new, created_at=Now, updated_at=Now}],
    Job = #pushy_job{id=JobId, org_id=?ORGID, job_nodes=JobNodes,
                     command=?COMMAND, quorum=1, run_timeout=1, status=new,
                     opts=Opts, last_updated_by= <<"updater">>,
                     created_at=Now, updated_at=Now},
    JobPid = run_job_with_result(NodePid, Job, NodeRef, Result),
    {JobPid, JobId}.

run_job_with_result(NodePid, Job, NodeRef, Result) ->
    {ok, JobPid} = pushy_job_state:start_link(Job, ?REQUESTOR),
    JobId = Job#pushy_job.id,
    erlang:unlink(JobPid),
    % "respond" to the commit messsage we know is sent
    send_msg(NodeRef, NodePid, JobId, ack_commit),
    % "respond" to the run messsage we know is sent
    send_msg(NodeRef, NodePid, JobId, ack_run),
    % claim to have succeeded at running the command
    % XX Note that 500ms is too small, as the delay between ibrowse:send_req
    % and the webmachine code initializing the resource can be 250ms, and the
    % processing after that can be another 250ms.  I'm currently unclear where
    % all the delays are (although there's definitely some poor code in
    % webmachine, e.g. a single resource request will call
    % content_types_provided/2 multiple times.  But some may also be in
    % mochiweb, or even ibrowse.
    {ResultType, ResultParams} = Result,
    timer:apply_after(750, ?MODULE, send_msg, [NodeRef, NodePid, JobId,
                                               ResultType, ResultParams]),
    wait_for_gproc_exists({n, l, {pushy_job, JobId}}),
    JobPid.

get_full_path(OrgName, Path) ->
    iolist_to_binary(io_lib:format("/organizations/~s/pushy/~s",
                                   [OrgName, Path])).

get_feed(CreatorName, OrgName, Path, AsyncReceiver, LastEventId) when
                                                    is_binary(LastEventId) ->
    get_feed(CreatorName, OrgName, Path, AsyncReceiver,
             binary_to_list(LastEventId));
get_feed(CreatorName, OrgName, Path, AsyncReceiver, LastEventId) ->
    LEHeader = case LastEventId of
                   no_last_event_id -> [];
                   _ -> [{"Last-Event-ID", LastEventId}]
               end,
    Headers = [{"Accept", "text/event-stream"} | LEHeader],
    api_request(get, CreatorName, OrgName, Path, AsyncReceiver,
                Headers, <<"">>).

api_request(Method, CreatorName, OrgName, Path, AsyncReceiver, ExtraHeaders, Body) ->
    BMethod = case Method of
                  get -> <<"GET">>;
                  post -> <<"POST">>
              end,
    FullPath = get_full_path(OrgName, Path),
    {ok, PrivateKey} = chef_keyring:get_key(pushy_priv),
    Headers = chef_authn:sign_request(PrivateKey, Body, CreatorName, BMethod, now, FullPath),
    FullHeaders = Headers ++ ExtraHeaders,
    Url = "http://localhost:10003" ++ binary_to_list(FullPath),
    case AsyncReceiver of
        synchronous -> sync_http_request(Method, Url, FullHeaders, Body);
        _ -> async_http_request(Method, Url, FullHeaders, Body, AsyncReceiver)
    end.

get_job_feed(AsyncReceiver, JobId) ->
    Path = io_lib:format("job_status_feed/~s", [JobId]),
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

get_job(JobId, Org) ->
    Path = iolist_to_binary(io_lib:format("~s/~s", ["jobs", JobId])),
    {ok, Code, _ResHeaders, ResBody} = api_request(get, <<"creator_name">>, Org, Path, synchronous, [], <<"">>),
    ?assertEqual("200", Code),
    jiffy:decode(ResBody).

get_org_jobs(Org) ->
    {ok, Code, _ResHeaders, ResBody} = api_request(get, <<"creator_name">>, Org, "jobs", synchronous, [], <<"">>),
    ?assertEqual("200", Code),
    jiffy:decode(ResBody).

post_job(Command, Nodes, OptFields) ->
    JsonObj = [{command, Command}, {nodes, Nodes}] ++ OptFields,
    Headers = [{"Content-Type", "application/json"}],
    {ok, Code, ResHeaders, ResBody} = api_request(post, <<"creator_name">>, ?ORG, "jobs", synchronous, Headers, jiffy:encode({JsonObj})),
    case Code of
        "201" ->
            JobId = lists:last(re:split(proplists:get_value("Location", ResHeaders), "/")),
            {ok, JobId};
        _ -> {error, Code, ResBody}
    end.

sync_http_request(Method, Url, Headers, Body) ->
    Res = ibrowse:send_req(Url, Headers, Method, Body, [{response_format, ?RESPONSE_FORMAT}]),
    case Res of
        {ok, Code, ResHeaders, ResBody} -> {ok, Code, ResHeaders, list_to_binary(ResBody)};
        _ -> Res
    end.

async_http_request(Method, Url, Headers, Body, Receiver) ->
    % _have_ to use {stream_to, {Pid, once}} if we don't want the data to be accumulated in chunks of some byte-size,
    % but want to handle the data as it comes in.
    % Since we're turning on active-once, we have to remember to reset the "active-once" flag after every chunk of data
    % With long-lasting connections, we can't have the request in ibrowse's load-balancing pool.  So we will
    % spawn a worker process that we'll talk to directly.
    {ok, Worker} = ibrowse:spawn_link_worker_process(Url),
    {ibrowse_req_id, ReqId} = ibrowse:send_req_direct(Worker, Url, Headers, Method, Body,
                                                      [{stream_to, {Receiver, once}}, {response_format, ?RESPONSE_FORMAT}]),
    ReqId.

receive_async_headers(ReqId) ->
    Res = receive
            {ibrowse_async_headers, ReqId, C, Headers} -> {C, Headers}
          after
            100 ->
                  ?debugVal({no_async_header, ReqId}),
                  {error, no_async_header, ReqId}
    end,
    ibrowse:stream_next(ReqId),
    Res.

make_node_ref(OrgId, NodeName) -> {OrgId, NodeName}.

start_node_and_job() ->
    NodePid = start_node(),
    {JobPid, JobId} = run_default_job(NodePid),
    {NodePid, JobId, JobPid}.

stop_node_and_job(NodePid, JobId) ->
    stop_job(JobId),
    stop_node(NodePid).

stop_job(JobId) ->
    pushy_job_state:stop_job(JobId),
    wait_for_gproc_gone({n, l, {pushy_job, JobId}}).

stop_node(NodePid) -> stop_node(NodePid, make_node_ref(?ORGID, ?NODE)).

stop_node(NodePid, NodeRef) ->
    GprocName = pushy_node_state_sup:mk_gproc_name(NodeRef),
    NodePid ! should_die,
    wait_for_gproc_gone({n,l,GprocName}).

% Drat -- I'd prefer to use gproc:monitor, rather than polling, but that was introduced in a later versino of gproc.
wait_for_gproc_exists(P) ->
    case gproc:where(P) of
        undefined -> timer:sleep(50), wait_for_gproc_exists(P);
        _ ->  ok
    end.

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
    {PL} = jiffy:decode(V),
    Timestamp = case lists:keyfind(<<"timestamp">>, 1, PL) of
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
    case io_lib:fread("~4d-~2d-~2d ~2d:~2d:~2d.~6dZ", binary_to_list(S)) of
        {ok, [Year, Month, Day, Hour, Minute, Second, USec], _} ->
            {{Year, Month, Day}, {Hour, Minute, Second}, USec}
    end.

atom_to_binary(A) -> list_to_binary(atom_to_list(A)).

get_ev_val(Ev, Key) when is_atom(Key) -> get_ev_val(Ev, atom_to_binary(Key));
get_ev_val(Ev, Key) when is_binary(Key) ->
    case proplists:get_value(Key, Ev#event.data) of
        undefined ->
            ?debugVal({no_such_key, Key, Ev}),
            ?assert(false);
        Val -> Val
    end.

no_ev_val(Ev, Key) when is_atom(Key) -> no_ev_val(Ev, atom_to_binary(Key));
no_ev_val(Ev, Key) when is_binary(Key) ->
    case proplists:get_value(Key, Ev#event.data) of
        undefined -> true;
        Val ->
            ?debugVal({unexpected_key, Key, Val, Ev}),
            false
    end.

validate_events(NumEvents, Evs) ->
    case length(Evs) of
        NumEvents -> validate_event_content(Evs);
        _ ->
            ?debugVal(Evs),
            ?assertEqual(NumEvents, length(Evs))
    end.

validate_event_content(Evs) ->
    % Events have unique ids
    ?assertEqual(length(Evs), length(lists:usort([E#event.id || E <- Evs]))),
    % Events have unique timestamps, and are in order
    Ts = [E#event.timestamp || E <- Evs],
    ?assertEqual(Ts, lists:usort(Ts)).

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
    ?assertEqual(JobId, get_ev_val(Ev, job)),
    ?assertEqual(?COMMAND, get_ev_val(Ev, command)),
    ?assertEqual(1, get_ev_val(Ev, run_timeout)),
    ?assertEqual(1, get_ev_val(Ev, quorum)),
    ?assertEqual(1, get_ev_val(Ev, node_count)),
    ?assertEqual(?REQUESTOR, get_ev_val(Ev, user)).

expect_org_job_complete(Ev, JobId, Status) ->
    ?assertEqual(job_complete, Ev#event.name),
    ?assertEqual(JobId, get_ev_val(Ev, job)),
    ?assertEqual(atom_to_binary(Status), get_ev_val(Ev, status)).

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

% Same as string:str/2, except works on binaries.
binary_str(StrB, SubStrB) -> string:str(binary_to_list(StrB), binary_to_list(SubStrB)).
