%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author John Keiser <jkeiser@opscode.com>
%%% @doc
%%% REST resource for creating and listing push jobs
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

-include_lib("ej/include/ej.hrl").

-include_lib("eunit/include/eunit.hrl").

init(Config) ->
    pushy_wm_base:init(Config).

%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

malformed_request(Req, State) ->
    malformed_request(wrq:method(Req), Req, State).

malformed_request('GET', Req, State) ->
    pushy_wm_base:malformed_request(Req, State);
malformed_request('POST', Req, State) ->
    try
        % Note: must call pushy_wm_base:malformed_request first, because it will prevent
        % a too-large body from being completely retrieved.
        Resp = pushy_wm_base:malformed_request(Req, State),
        case Resp of
            {false, Req0, State} ->
                validate_request(Req0),
                Resp;
            _ -> Resp
        end
    catch
        throw:bad_command ->
            Msg = <<"invalid command supplied">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_nodes ->
            Msg = <<"at least one node must be supplied">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_key ->
            Msg = <<"invalid key supplied">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_type_quorum ->
            Msg = <<"invalid type supplied for quorum (expected a number)">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_type_run_timeout ->
            Msg = <<"invalid type supplied for run_timeout (expected a number)">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:dup_env_keys ->
            Msg = <<"invalid environment value (duplicate key)">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_file ->
            Msg = <<"invalid file contents (expected 'base64:...' or 'raw:...')">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_base64 ->
            Msg = <<"invalid file contents (couldn't decode base64 string)">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:unknown_validation_error ->
            Msg = <<"an unknown validation error has occurred">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State};
        throw:bad_file_len ->
            Msg = io_lib:format("invalid file size (must be <= ~p bytes)", [?MAX_FILE_SIZE]),
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 400}, Req1, State}
    end.


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
    {Command, NodeNames, RunTimeout, Quorum, User, Dir, Env, File} = parse_post_body(Req),
    RunTimeout2 = case RunTimeout of
        undefined -> envy:get(pushy, default_running_timeout, 3600, integer);
        Value -> Value
    end,
    Job = pushy_object:new_record(pushy_job, OrgId, NodeNames, Command, RunTimeout2, Quorum, User, Dir, Env, File),
    State2 = State#config_state{pushy_job = Job},
    {binary_to_list(Job#pushy_job.id), Req, State2}.

% This processes POST /pushy/jobs
from_json(Req, #config_state{pushy_job = Job, organization_name = OrgName, requestor = Requestor} = State) ->
    ok = pushy_job_state_sup:start(Job, Requestor),
    Req2 = set_uri_of_created_resource(Req, OrgName),
    {true, Req2, State}.

%% GET /pushy/jobs
to_json(Req, #config_state{organization_guid = OrgId} = State) ->
    {ok, Jobs} = pushy_sql:fetch_jobs(OrgId),
    EJson = assemble_jobs_ejson(Jobs),
    {jiffy:encode(EJson), Req, State}.

% Private stuff

validate_request(Req) ->
    Body = wrq:req_body(Req),
    JobJson = jiffy:decode(Body),
    validate_body(JobJson).

validate_body(Json) ->
    Spec = {[{<<"command">>, string},
             {<<"nodes">>, {array_map, string}},
             {{opt, <<"run_timeout">>}, number},
             {{opt, <<"quorum">>}, number},
             {{opt, <<"user">>}, string},
             {{opt, <<"dir">>}, string},
             {{opt, <<"env">>}, {object_map, {{keys, string}, {values, string}}}},
             {{opt, <<"file">>}, string}
            ]},

    case ej:valid(Spec, Json) of
        ok ->
            %% Make sure there are no extra keys
            validate_keys(Json),
            %% Make sure there is at least one node in the array
            validate_nodes(ej:get({<<"nodes">>}, Json)),
            %% Make sure env values are of the right form
            validate_env(ej:get({<<"env">>}, Json)),
            %% Make sure file is of the right form
            validate_file(ej:get({<<"file">>}, Json)),
            ok;
        #ej_invalid{type = Type, key = Key} ->
            case {Type, Key} of
                {_, <<"command">>} -> throw(bad_command);
                {_, <<"nodes">>} -> throw(bad_nodes);
                {json_type, <<"run_timeout">>} -> throw(bad_type_run_timeout);
                {json_type, <<"quorum">>} -> throw(bad_type_quorum);
                {_T, _K} -> throw(unknown_validation_error)
            end
    end.

validate_nodes(Nodes) ->
    if
        length(Nodes) =:= 0 -> throw(bad_nodes);
        true -> true
    end.

validate_keys({Json}) ->
    ExpectedKeys = [<<"command">>, <<"nodes">>, <<"run_timeout">>, <<"quorum">>,
                    <<"user">>, <<"dir">>, <<"env">>, <<"file">>],
    Keys = proplists:get_keys(Json),

    case Keys -- ExpectedKeys of
        [] -> true;
        _ -> throw(bad_key)
    end.

validate_env(undefined) -> ok;
validate_env({Es}) ->
    Keys = [E || {E, _K} <- Es],
    case length(Keys) - sets:size(sets:from_list(Keys)) of
        0 -> ok;
        _ -> throw(dup_env_keys)
    end.

% We validate that it has a prefix of either "raw:" or "base64:", and if base64, that it is
% a valid encoding.  Other than that, we just pass it around (in the database, and in the protocol).
validate_file(undefined) -> ok;
validate_file(<<"base64:", B64/binary>>) -> 
    try
        S = base64:decode(B64),
        validate_file_length(S)
    catch
        error:_ -> throw(bad_base64)
    end;
validate_file(<<"raw:", S/binary>>) -> validate_file_length(S);
validate_file(_) -> throw(bad_file).

validate_file_length(S) when size(S) > ?MAX_FILE_SIZE -> throw(bad_file_len);
validate_file_length(_) -> ok.

assemble_jobs_ejson(Jobs) ->
    [ pushy_object:assemble_job_ejson(Job) || Job <- Jobs ].

parse_post_body(Req) ->
    Body = wrq:req_body(Req),
    JobJson = jiffy:decode(Body),
    Command = ej:get({<<"command">>}, JobJson),
    NodeNames = ej:get({<<"nodes">>}, JobJson),
    RunTimeout = ej:get({<<"run_timeout">>}, JobJson),
    Quorum = ej:get({<<"quorum">>}, JobJson, length(NodeNames)),
    User = ej:get({<<"user">>}, JobJson),
    Dir = ej:get({<<"dir">>}, JobJson),
    Env = ej:get({<<"env">>}, JobJson),
    File = ej:get({<<"file">>}, JobJson),
    { Command, NodeNames, RunTimeout, Quorum, User, Dir, Env, File }.

%% TODO The below are MERCILESSLY STOLEN FROM chef_rest.  Get this in a common place, yo.
%% @doc Sets the JSON body of a response and it's Location header to
%% point to the URI of a newly-created resource.
%%
%% The body will be of the form
%%
%%     {"uri":"http://foo.com/newresource"}
%%
%% Returns the updated request.
set_uri_of_created_resource(Req, OrgName) ->
    set_uri(full_uri(Req, OrgName), Req).

set_uri(Uri, Req) when is_list(Uri) ->
    set_uri(list_to_binary(Uri), Req);
set_uri(Uri, Req0) when is_binary(Uri) ->
    %% Uri needs to be a binary for encoding to JSON, but a string for the header value
    Req = set_json_body(Req0, {[{<<"uri">>, Uri}]}),
    wrq:set_resp_header("Location", binary_to_list(Uri), Req).

%% @doc Converts the given json-encoded data to a JSON string and
%% sets it as the request body, returning the updated request.
%% @end
set_json_body(Req, EjsonData) ->
    Json = jiffy:encode(EjsonData),
    wrq:set_resp_body(Json, Req).

-spec base_uri(#wm_reqdata{}) -> string().
%% @doc Returns the base URI for the server as called by the client as a string.
base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(wrq:host_tokens(Req), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

full_uri(Req, OrgName) ->
    base_uri(Req) ++ "/organizations/" ++ binary_to_list(OrgName) ++ "/pushy/jobs/" ++
        wrq:disp_path(Req).

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http";
                P -> erlang:atom_to_list(P)
            end;
        Proto -> Proto
    end.

%% So this is kind of gross and will prevent correct port info if you run https on port 80
%% or http on port 443; otherwise it should work. The problem is two-fold, first webmachine
%% ignores scheme information when parsing the host header and so always sets the port to 80
%% if no port is present in the host header. But in a load-balanced situation, the scheme
%% from webmachine may not reflect what is in use at the load balancer. A simple compromise
%% is to treat both 80 and 443 as default and only include a port string if the port differs
%% from those.
port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].
