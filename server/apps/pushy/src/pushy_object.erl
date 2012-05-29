%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General utility module for common functions that operate on
%% "Chef Objects", such as nodes, roles, etc.
-module(pushy_object).

-include("pushy_sql.hrl").

-export([
          create_object/3,
          update_object/3,
          new_record/3
        ]).

new_record(pushy_node_status, OrgId, NodeStatusData) ->
    Name = proplists:get_value(<<"node">>, NodeStatusData),
    Status = proplists:get_value(<<"type">>, NodeStatusData),
    #pushy_node_status{org_id = OrgId,
                      node_name = Name,
                      status = Status
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

?MAKE_SET_UPDATED(pushy_node_status).

%% Similar to MAKE_SET_UPDATED, a helper for generating a set_created function that sets
%% created_at, updated_at, and last_updated_by on any of the Chef object types.
-define(MAKE_SET_CREATED(Rec),
        set_created(#Rec{}=Object, ActorId) ->
               Now = sql_date(now),
               Object#Rec{created_at = Now,
                          updated_at = Now, last_updated_by = ActorId}).

?MAKE_SET_CREATED(pushy_node_status).

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
