%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>

%% @doc General utility module for common functions that operate on
%% "Pushy Objects", such as node_status, jobs, etc.
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
-module(pushy_object).

-include("pushy.hrl").
-include("pushy_sql.hrl").

-export([
          fetch_org_id/1,
          create_object/3,
          update_object/2,
          update_object/3,
          new_job_record/2,

          make_org_prefix_id/1,
          make_org_prefix_id/2,

          assemble_job_ejson/1,
          assemble_job_ejson_with_nodes/1,
          assemble_job_ejson_with_nodes/2

        ]).

fetch_org_id(OrgName) ->
    case pushy_cache:get(org_guid, OrgName) of
        Error when Error =:= not_found orelse Error =:= no_cache ->
            case pushy_org:fetch_org_id(OrgName) of
                not_found ->
                    not_found;
                Guid ->
                    pushy_cache:put(org_guid, OrgName, Guid),
                    Guid
            end;
        {ok, Guid} ->
            Guid
    end.

-spec new_job_record(object_id(), #job_create_desc{}) -> pushy_object().
new_job_record(OrgId, Desc) ->
    #job_create_desc{command = Command, node_names = NodeNames,
       run_timeout = RunTimeout, quorum = Quorum, user = User,
       dir = Dir, env = Env, file = File, capture = Capture} = Desc,
    Id = make_org_prefix_id(OrgId),
    Now = pushy_sql:sql_date(now),
    Opts = #pushy_job_opts{
                user = User,
                dir = Dir,
                env = Env,
                file = File,
                capture = Capture
             },
    #pushy_job{id = Id,
                org_id = OrgId,
                status = new,
                command = Command,
                run_timeout = RunTimeout,
                quorum = Quorum,
                opts = Opts,
                created_at = Now,
                updated_at = Now,
                job_nodes = [
                  #pushy_job_node{job_id = Id,
                                  org_id = OrgId,
                                  node_name = NodeName,
                                  status = new,
                                  created_at = Now,
                                  updated_at = Now} ||
                                  NodeName <- NodeNames]
                }.

%% CHEF_COMMON CARGO_CULT

%% @doc Generic object creation.  `Fun' is the function in the `pushy_sql'
%% module to use for the creation and determines the return type (will return the
%% appropriate pushy object record type).
create_object(Fun, Object, ActorId) ->
    Object1 = set_created(Object, ActorId),
    pushy_sql:Fun(Object1).

%% @doc Generic update for Pushy object types. `Fun' is the appropriate function in the
%% `pushy_sql' module. `Object' is a Pushy object (record) with updated data.
update_object(Fun, Object, ActorId) ->
    Object1 = set_updated(Object, ActorId),
    pushy_sql:Fun(Object1).

update_object(Fun, Object) ->
    Object1 = set_updated(Object),
    pushy_sql:Fun(Object1).

%% CHEF_COMMON CARGO_CULT
%% chef_db ?MAKE_SET_UPDATED

%% To avoid some of the repetition of defining a function to set updated_at and
%% last_updated_by across our different chef_object record types, we define a macro that
%% expands to the function definition.
%%
%% Here's a place where something like the ct_expand parse_transform would be really nice.
%% NOTE: a downside of this setup is that you can't easily jump to the definition of
%% `set_updated' using standard emacs code tools.
-define(MAKE_SET_UPDATED_WITH_ACTOR(Rec),
        set_updated(#Rec{}=Object, ActorId) ->
            Object#Rec{updated_at = pushy_sql:sql_date(now), last_updated_by = ActorId}).

-define(MAKE_SET_UPDATED(Rec),
        set_updated(#Rec{}=Object) ->
            Object#Rec{updated_at = pushy_sql:sql_date(now)}).

?MAKE_SET_UPDATED_WITH_ACTOR(pushy_job).

?MAKE_SET_UPDATED(pushy_job_node).

%% Similar to MAKE_SET_UPDATED, a helper for generating a set_created function that sets
%% created_at, updated_at, and last_updated_by on any of the Chef object types.
-define(MAKE_SET_CREATED(Rec),
        set_created(#Rec{}=Object, ActorId) ->
               Now = pushy_sql:sql_date(now),
               Object#Rec{created_at = Now,
                          updated_at = Now, last_updated_by = ActorId}).

?MAKE_SET_CREATED(pushy_job).

%% CHEF_COMMON CARGO_CULT
%% chef_db:make_org_prefix_id/1
%% chef_db:make_org_prefix_id/2

%% @doc
%% Create a GUID with an org-specifc prefix for nameless objects.
%%
%% See chef_db:make_org_prefix_id/2 for further details.
-spec make_org_prefix_id(object_id()) -> object_id().
make_org_prefix_id(OrgId) ->
    %% The GUIDs we generate incorporate an object's name. Most times, the objects we'll
    %% want to create GUIDs for will have names of their own; this is not the case for
    %% jobs, at least, which are only identified by a GUID. To keep things simple,
    %% we'll just generate a random "name" for them, and then pass this along to the
    %% "normal" GUID creation machinery.
    %%
    %% It will still be prefixed with an org-specific prefix, though, just like our other
    %% GUIDs.
    FakeName = crypto:rand_bytes(32), %% Picked 32 for the hell of it
    make_org_prefix_id(OrgId, FakeName).

-spec make_org_prefix_id(object_id(), string()|binary()) -> object_id().
%% @doc Create a guid with org-specific prefix
%%
%% We use the last 48 bits of the org guid as the prefix for the object guid. The remainder
%% of the object guid is the first 80 bits of the MD5 hash of org name, object name, and six
%% random bytes.
%%
%% We could also add in object type, but the random bytes should take care of same name
%% different type situations just as well and also solves race condition issues with
%% multiple requests for the same name.
%%
make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = crypto:hash(md5, Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

assemble_job_ejson_with_nodes(Job) -> assemble_job_ejson_with_nodes(Job, false).

assemble_job_ejson_with_nodes(#pushy_job{job_nodes = Nodes} = Job, IncludeFile) ->
    {NodePropList} = assemble_job_ejson(Job, IncludeFile),
    NodesJson = job_nodes_json_by_status(Nodes),
    {[ {<<"nodes">>, NodesJson} | NodePropList]}.

get_attr_list(Name, Val) -> get_attr_list(Name, Val, undefined).

get_attr_list(_Name, Default, Default) -> [];
get_attr_list(Name, Val, _Default) -> [{Name, Val}].

assemble_job_ejson(Job) -> assemble_job_ejson(Job, false).

assemble_job_ejson(#pushy_job{id = Id,
                              command = Command,
                              status = Status,
                              run_timeout = RunTimeout,
                              created_at = CreatedAt,
                              updated_at = UpdatedAt,
                              opts = Opts},
                  IncludeFile) ->
    % XXX This code is copied from pushy_job_state.erl
    UserPL = get_attr_list(user, Opts#pushy_job_opts.user),
    DirPL = get_attr_list(dir, Opts#pushy_job_opts.dir),
    EnvPL = get_attr_list(env, Opts#pushy_job_opts.env),
    CapturePL = get_attr_list(capture_output, Opts#pushy_job_opts.capture, false),
    FilePL = case Opts#pushy_job_opts.file of
                undefined -> [];
                File -> case IncludeFile of
                            true -> [{file, File}];
                            false -> [{file_specified, true}]
                        end
             end,
    OptsPL = UserPL ++ DirPL ++ EnvPL ++ CapturePL ++ FilePL,
    {[ {<<"id">>, Id},
       {<<"command">>, Command},
       {<<"status">>, Status},
       {<<"run_timeout">>, RunTimeout},
       {<<"created_at">>, CreatedAt},
       {<<"updated_at">>, UpdatedAt}
    ] ++ OptsPL}.

job_nodes_json_by_status(Nodes) ->
    NodesByStatus = job_nodes_by_status(Nodes, dict:new()),
    {[
        { erlang:atom_to_binary(Status, utf8), dict:fetch(Status, NodesByStatus) }
        || Status <- dict:fetch_keys(NodesByStatus)
    ]}.

job_nodes_by_status([], Dict) ->
    Dict;
job_nodes_by_status([#pushy_job_node{node_name = Name, status = Status} | Nodes], Dict) ->
    Dict2 = dict:append(Status, Name, Dict),
    job_nodes_by_status(Nodes, Dict2).

