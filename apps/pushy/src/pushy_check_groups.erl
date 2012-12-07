%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Doug Triggs <doug@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc check to see if requestor is in a group
-module(pushy_check_groups).

-include_lib("eunit/include/eunit.hrl").

-export([
          group_membership/4
        ]).

%% @doc determine if a requestor is in a group, not in a group, or the group
%% doesn't exist (returns undefined)
-spec group_membership(Name :: string(), Type :: string(), OrgName :: string(),
                       Group :: string()) -> true | false | group_not_found.
group_membership(Name, Type, OrgName, Group) ->
    Path = path(OrgName, Group),
    case pushy_http_common:fetch_authenticated(Path) of
        not_found ->
            group_not_found;
        ResponseBody ->
            check_for_membership(Name, Type, OrgName, ResponseBody)
    end.

path(OrgName, Group) ->
    "/organizations/" ++ OrgName ++ "/groups/" ++ Group.

%%
%% Internal functions
%%

%% @doc determine if a member of the group, or recurse if necessary
%%
check_for_membership(Name, Type, OrgName, Body) ->
    EJson = jiffy:decode(Body),
    Reqs = case Type of
               user ->
                   ej:get({<<"users">>}, EJson);
               client ->
                   ej:get({<<"clients">>}, EJson)
           end,
    case lists:member(list_to_binary(Name), Reqs) of
        true ->
            true;
        false ->
            Groups = ej:get({<<"groups">>}, EJson),
            case check_subgroups(Name, Type, OrgName, Groups) of
                true ->
                    true;
                false ->
                    false
            end
    end.

%% @doc recurse over subgroups
check_subgroups(_, _, _, []) ->
    false;
check_subgroups(Name, Type, OrgName, [Group|OtherGroups]) ->
    case group_membership(Name, Type, OrgName, Group) of
        true ->
            true;
        _ ->
            check_subgroups(Name, Type, OrgName, OtherGroups)
    end.
