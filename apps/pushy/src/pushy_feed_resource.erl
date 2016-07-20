%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Steven Grady <steven.grady@erlang-solutions.com>
%%% @doc
%%% REST resource providing server-sent event stream for jobs
%%% @end
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
-module(pushy_feed_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         resource_exists/2,
         to_event_stream/2]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_sql.hrl").
-include("pushy_event.hrl").
-include("pushy_wm.hrl").

-compile([{parse_transform, lager_transform}]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    pushy_wm_base:init(Config).
    %{ok, C} = pushy_wm_base:init(Config), {{trace, "/tmp/traces"}, C}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:10003/wmtrace

malformed_request(Req, State) ->
    pushy_wm_base:malformed_request(Req, State).

is_authorized(Req, State) ->
    pushy_wm_base:is_authorized(Req, State).

forbidden(Req, State) ->
    pushy_wm_base:read_forbidden(Req, State).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"text/event-stream", to_event_stream}], Req, State}.

resource_exists(Req, State) ->
    case wrq:path_info(job_id, Req) of
        undefined ->
            % When asking for the org feed, if we've gotten this far, then everything's fine
            {true, Req, State};
        JobIdStr ->
            JobId = list_to_binary(JobIdStr),
            case pushy_job_state_sup:get_process(JobId) of
                not_found ->
                    case pushy_sql:fetch_job(JobId) of
                        {ok, not_found} -> {false, Req, State};
                        {ok, Job} -> {true, Req, State#config_state{pushy_job = Job}}
                    end;
                JobPid -> {true, Req, State#config_state{job_pid = JobPid}}
            end
    end.

process_job_msg(Msg, MonitorRef) ->
    case Msg of
        {job_ev, _JobId, Ev} ->
            {ok, [encode_event(Ev), "\n\n"]};
        done ->
            case MonitorRef of
                undefined -> ok;
                _ -> demonitor(MonitorRef, [flush])
            end,
            {ok, done};
        {'DOWN', MonitorRef, process, JobPid, Info} ->
            case Info of
                normal -> ok;
                shutdown -> ok;
                _ -> lager:error("Job ~p exited: ~p", [JobPid, Info])
            end,
            % XXX Post a job-complete, if one wasn't provided?
            {ok, done};
        _ ->
            unknown
    end.

process_org_msg(Msg) ->
    case Msg of
        {ev, Ev} ->
            {ok, [encode_event(Ev), "\n\n"]};
        _ ->
            unknown
    end.

stream_events(MsgProcessorFun, KeepAliveTime) ->
    Resp = receive
        ping ->
            % Solely for testing: provide a mechanism for a test to cause data to be
            % be sent, because there is no way to detect that an idle connection has
            % been closed (due to a limitation of webmachine).
            <<":ping\n">>;
        Msg ->
            case MsgProcessorFun(Msg) of
                {ok, R} -> R;
                unknown ->
                    lager:error("Unexpected message: ~p", [Msg]),
                    {ok, <<>>}
            end
    after
        KeepAliveTime -> <<":keepalive\n">>
    end,
    case Resp of
        done -> {<<>>, done};
        {_R, done} -> Resp;
        _ ->
            %?debugVal(binary_to_list(iolist_to_binary(Resp))),
            {Resp, fun() -> stream_events(MsgProcessorFun, KeepAliveTime) end}
    end.

encode_event(#event{name = Name, id = Id, timestamp = Timestamp, data = Data}) ->
    Props = [{<<"timestamp">>, list_to_binary(pushy_event:get_time_as_iso8601(Timestamp))} | Data],
    pushy_fsm_utils:intersperse("\n", [[<<"event: ">>, Name], [<<"id: ">>, Id], [<<"data: ">>, jiffy:encode({Props})]]).

encode_events(Es) ->
    pushy_fsm_utils:intersperse("\n\n", [encode_event(E) || E <- Es]).

% Also subscribes to future events from job
get_job_events(JobPid, LastEventID) ->
    {Done, Evs} = pushy_job_state:get_events(JobPid, LastEventID),
    EvStr = encode_events(Evs),
    FinalStr = case Done of
        true -> EvStr;
        false -> [EvStr, "\n\n"]
    end,
    {Done, FinalStr}.

% Also subscribes to future events from org
get_org_events(OrgId, LastEventID) ->
    Evs = pushy_org_events:get_events(OrgId, LastEventID),
    case Evs of
        [] -> [];
        _ -> [encode_events(Evs), "\n\n"]
    end.

to_event_stream(Req, State) ->
    LastEventID = wrq:get_req_header("Last-Event-ID",Req),
    KeepAliveTime = envy:get(pushy, keep_alive_time, 15, integer),
    Res = case {wrq:path_info(job_id, Req), State#config_state.pushy_job} of
        {undefined, undefined} ->
            OrgId = State#config_state.organization_guid,
            Events = get_org_events(OrgId, LastEventID),
            Stream = {Events, fun() -> stream_events(fun process_org_msg/1, KeepAliveTime * 1000) end},
            {stream, Stream};
        {_, undefined} ->
            JobPid = State#config_state.job_pid,
            Stream = case get_job_events(JobPid, LastEventID) of
                {true, Events} ->
                    {Events, done};
                {false, Events} ->
                    %?debugVal(binary_to_list(iolist_to_binary(Events))),
                    % We've automatically been subscribed to events
                    MonitorRef = monitor(process, JobPid),
                    {Events, fun() -> stream_events(fun(Msg) ->  process_job_msg(Msg, MonitorRef) end, KeepAliveTime * 1000) end}
            end,
            {stream, Stream};
        {_, Job} ->
            encode_event(pushy_job_state:make_job_summary_event(Job))
    end,
    %?debugVal(Res),

    %% Nginx buffers output, but this header disables.
    %% Alternate approach would be to disable buffering for endpoint in nginx config...
    %% https://stackoverflow.com/questions/27898622/server-sent-events-stopped-work-after-enabling-ssl-on-proxy
    %% https://stackoverflow.com/questions/20106386/server-sent-events-eventsource-with-sinatra-on-elastic-beanstalk
    Req2 = wrq:set_resp_headers([{"X-Accel-Buffering", "no"}], Req),
    {Res, Req2, State}.
