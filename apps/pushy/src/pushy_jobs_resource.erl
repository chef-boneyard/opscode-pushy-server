%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author John Keiser <jkeiser@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource for creating and listing push jobs
%%% @end
-module(pushy_jobs_resource).

-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         forbidden/2,
         from_json/2,
         is_authorized/2,
         malformed_request/2,
         post_is_create/2,
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
    forbidden(wrq:method(Req), Req, State).
forbidden('POST', Req, State) ->
    pushy_wm_base:write_forbidden(Req, State);
forbidden('GET', Req, State) ->
    pushy_wm_base:read_forbidden(Req, State).

allowed_methods(Req, State) ->
    {['POST', 'GET'], Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

% {
%   'command' = 'chef-client',
%   'nodes' = [ 'DERPY', 'RAINBOWDASH' ]
% }

post_is_create(Req, State) ->
    {true, Req, State}.

% This creates the job record
create_path(Req, #config_state{organization_guid = OrgId} = State) ->
    {Command, NodeNames, RunTimeout, Quorum} = parse_post_body(Req),
    RunTimeout2 = case RunTimeout of
        undefined -> envy:get(pushy, default_running_timeout, 3600, integer);
        Value -> Value
    end,
    Job = pushy_object:new_record(pushy_job, OrgId, NodeNames, Command, RunTimeout2, Quorum),
    State2 = State#config_state{pushy_job = Job},
    {binary_to_list(Job#pushy_job.id), Req, State2}.

% This processes POST /pushy/jobs
from_json(Req, State) ->
    pushy_job_state_sup:start(State#config_state.pushy_job),
    Req2 = ripped_from_chef_rest:set_uri_of_created_resource(Req),
    {true, Req2, State}.

%% GET /pushy/jobs
to_json(Req, #config_state{organization_guid = OrgId} = State) ->
    {ok, Jobs} = pushy_sql:fetch_jobs(OrgId),
    {jiffy:encode(Jobs), Req, State}.

% Private stuff

parse_post_body(Req) ->
    Body = wrq:req_body(Req),
    JobJson = jiffy:decode(Body),
    Command = ej:get({<<"command">>}, JobJson),
    NodeNames = ej:get({<<"nodes">>}, JobJson),
    RunTimeout = ej:get({<<"run_timeout">>}, JobJson),
    Quorum = ej:get({<<"quorum">>}, JobJson, length(NodeNames)),
    { Command, NodeNames, RunTimeout, Quorum }.
