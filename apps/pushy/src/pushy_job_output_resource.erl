%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Steven Grady <steven.grady@erlang-solutions.com>
%%% @doc
%%% REST resource providing the output blobs from jobs
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
-module(pushy_job_output_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         resource_exists/2,
         to_byte_stream/2]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_sql.hrl").
-include("pushy_event.hrl").
-include("pushy_wm.hrl").

-compile([{parse_transform, lager_transform}]).

-include_lib("webmachine/include/webmachine.hrl").

init(InitArgs) ->
    {ok, Config} = pushy_wm_base:init(InitArgs),
    case proplists:get_value(output, InitArgs) of
        undefined -> {error, output_unspecified};
        stdout -> {ok, Config#config_state{output = stdout}};
        stderr -> {ok, Config#config_state{output = stderr}};
        Other -> {error, {invalid_output, Other}}
    end.

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
    {[{"application/octet-stream", to_byte_stream}], Req, State}.

resource_exists(Req, State) ->
    JobId = list_to_binary(wrq:path_info(job_id, Req)),
    NodeName = list_to_binary(wrq:path_info(node_name, Req)),
    Channel = State#config_state.output,
    case pushy_sql:fetch_job_output(JobId, NodeName, Channel) of
        {ok, Output} -> {true, Req, State#config_state{output_data = Output}};
        _ -> {false, Req, State}
    end.

to_byte_stream(Req, State) ->
    {State#config_state.output_data, Req, State}.
