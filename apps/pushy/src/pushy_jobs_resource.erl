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
         is_authorized/2,
         from_json/2,
         post_is_create/2,
         create_path/2]).

-include("pushy_sql.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {
          organization_guid :: string(),
          job_id :: binary() | undefined
        }).

init(_Config) ->
    % ?debugVal(_Config),
    State = #config_state{},
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

is_authorized(Req, State) ->
    OrgName =  wrq:path_info(organization_id, Req),
    %?debugVal(OrgName),
    State2 = State#config_state{organization_guid = pushy_object:fetch_org_id(OrgName) },
    {true, Req, State2}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", undef}], Req, State}.

% {
%   'command' = 'chef-client',
%   'nodes' = [ 'DERPY', 'RAINBOWDASH' ]
% }

post_is_create(Req, State) ->
    {true, Req, State}.

% This starts the job and returns its path
create_path(Req, #config_state{organization_guid = OrgId} = State) ->
    [ Command, NodeNames ] = parse_post_body(Req),
    JobId = pushy_job_state_sup:start(OrgId, Command, NodeNames),
    State2 = State#config_state{job_id = JobId},
    lager:info("Job ~p", [JobId]),
    lager:info("Job list ~p", [binary_to_list(JobId)]),
    {binary_to_list(JobId), Req, State2}.

% This processes POST /pushy/jobs (the work was already done in create_path)
from_json(Req, State) ->
    % TODO figure out what return value to expect
    Req2 = ripped_from_chef_rest:set_uri_of_created_resource(Req),
    {true, Req2, State}.

% Private stuff

parse_post_body(Req) ->
    Body = wrq:req_body(Req),
    JobJson = ejson:decode(Body),
    Command = ej:get({<<"command">>}, JobJson),
    NodeNames = ej:get({<<"nodes">>}, JobJson),
    [ Command, NodeNames ].
