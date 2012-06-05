%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General utility module for common functions that operate on
%% "Pushy Objects", such as node_status, jobs, etc.
-module(pushy_object).

-include("pushy_sql.hrl").

-export([
          create_object/3,
          update_object/3,
          new_record/3,

          make_org_prefix_id/1,
          make_org_prefix_id/2
        ]).

new_record(pushy_node_status, OrgId, NodeStatusData) ->
    Name = proplists:get_value(<<"node">>, NodeStatusData),
    Status = proplists:get_value(<<"type">>, NodeStatusData),
    #pushy_node_status{org_id = OrgId,
                      node_name = Name,
                      status = Status
                      };
new_record(pushy_job, OrgId, NodeNames) ->
    Id = chef_db:make_org_prefix_id(OrgId),
    #pushy_job{id = Id,
                org_id = OrgId,
                status = new,
                job_nodes = [
                  #pushy_job_node{job_id = Id,
                                  org_id = OrgId,
                                  node_name = NodeName,
                                  status = new,
                                  created_at = sql_date(now),
                                  updated_at = sql_date(now)} ||
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

%% CHEF_COMMON CARGO_CULT
%% chef_db ?MAKE_SET_UPDATED

%% To avoid some of the repetition of defining a function to set updated_at and
%% last_updated_by across our different chef_object record types, we define a macro that
%% expands to the function definition.
%%
%% Here's a place where something like the ct_expand parse_transform would be really nice.
%% NOTE: a downside of this setup is that you can't easily jump to the definition of
%% `set_updated' using standard emacs code tools.
-define(MAKE_SET_UPDATED(Rec),
        set_updated(#Rec{}=Object, ActorId) ->
               Object#Rec{updated_at = sql_date(now), last_updated_by = ActorId}).

?MAKE_SET_UPDATED(pushy_node_status);
?MAKE_SET_UPDATED(pushy_job).

%% Similar to MAKE_SET_UPDATED, a helper for generating a set_created function that sets
%% created_at, updated_at, and last_updated_by on any of the Chef object types.
-define(MAKE_SET_CREATED(Rec),
        set_created(#Rec{}=Object, ActorId) ->
               Now = sql_date(now),
               Object#Rec{created_at = Now,
                          updated_at = Now, last_updated_by = ActorId}).

?MAKE_SET_CREATED(pushy_node_status);
?MAKE_SET_CREATED(pushy_job).

%% CHEF_COMMON CARGO_CULT
%% chef_db:sql_date/1

%%%
%%% Emit in DATETIME friendly format
%%% TODO: Modify to generate datetime pseudo record as used by emysql?
sql_date(now) ->
    sql_date(os:timestamp());
sql_date({_,_,_} = TS) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second])).

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
    <<ObjectPart:80, _/binary>> = crypto:md5(Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).
