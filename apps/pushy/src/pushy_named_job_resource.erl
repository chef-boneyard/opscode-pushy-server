%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author John Keiser <jkeiser@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource for getting information about push jobs
%%% @end
-module(pushy_named_job_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         resource_exists/2,
         to_json/2]).

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

init(Config) ->
    pushy_wm_base:init(Config).
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

malformed_request(Req, State) ->
    pushy_wm_base:malformed_request(Req, State).

is_authorized(Req, State) ->
    pushy_wm_base:is_authorized(Req, State).

forbidden(Req, State) ->
    pushy_wm_base:read_forbidden(Req, State).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
    JobId = iolist_to_binary(wrq:path_info(job_id, Req)),
    case pushy_sql:fetch_job(JobId) of
        {ok, not_found} -> {false, Req, State};
        {ok, Job} -> {true, Req, State#config_state{pushy_job = Job}}
    end.

to_json(Req, #config_state{pushy_job = Job} = State) ->
    EJson = pushy_object:assemble_job_ejson_with_nodes(Job),
    {jiffy:encode(EJson), Req, State}.

