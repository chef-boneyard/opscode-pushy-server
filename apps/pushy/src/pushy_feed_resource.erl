%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
%% @copyright Copyright 2012-2012 Chef Software, Inc. All Rights Reserved.
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
-include("pushy_wm.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

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
    JobId = list_to_binary(wrq:path_info(job_id, Req)),
    case pushy_job_state_sup:get_process(JobId) of
        not_found ->
            case pushy_sql:fetch_job(JobId) of
                {ok, not_found} -> {false, Req, State};
                {ok, Job} -> {true, Req, State#config_state{pushy_job = Job}}
            end;
        _JobPid -> {true, Req, State}
    end.

% XXXX Hack until there's a way for Ruby to watch the stream, and for us to feed a stream.
await_job_complete(JobId) ->
    Evs = pushy_job_state:get_events(JobId),
    case string:str(binary_to_list(iolist_to_binary(Evs)), "job_complete") of 
        0 -> timer:sleep(100), await_job_complete(JobId);
        _ -> Evs
    end.

stream_events(JobId) ->
    receive
            {ev, Ev} -> {Ev, fun() -> stream_events(JobId) end};
            done -> {<<>>, done}
    end.

to_event_stream(Req, State) ->
    Res = case State#config_state.pushy_job of 
        undefined -> 
            JobId = list_to_binary(wrq:path_info(job_id, Req)),
            % XXX Does second look-up -- should store pid in state
            {Evs, Next} = case pushy_job_state:get_events(JobId) of
                not_found ->
                    {io_lib:format("error: could not find ~s", [JobId]), done};
                {true, Events} ->
                    {Events, done};
                {false, Events} ->
                    % We've automatically been subscribed to events
                    {Events, fun() -> stream_events(JobId) end}
            end,
            {stream, {Evs, Next}};
        Job ->
            pushy_job_state:make_job_summary_event(Job)
    end,
    {Res, Req, State}.
