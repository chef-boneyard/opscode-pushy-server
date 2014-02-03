%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Doug Triggs <doug@opscode.com>

%% @doc check to see if requestor is in a group
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
-module(pushy_check_groups).

-include("pushy_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
          group_membership/4
        ]).

%% @doc determine if a requestor is in a group, not in a group, or the group
%% doesn't exist (returns undefined)
-spec group_membership(Name :: string(),
                       Type :: pushy_requestor_type(),
                       OrgName :: string(),
                       Group :: string()) -> true | false | group_not_found.
group_membership(Name, Type, OrgName, Group) ->
    Path = path(OrgName, Group),
    try
        case pushy_http_common:fetch_authenticated(Path) of
            not_found ->
                group_not_found;
            ResponseBody ->
                check_for_membership(Name, Type, OrgName, ResponseBody)
        end
    catch
        % If server_error is thrown, something is wrong (e.g., organization may not
        % exist); forbid by default
        throw:{error, {server_error, _}} ->
            false
    end.

%%
%% Internal functions
%%

-spec path(OrgName :: string(), Group :: string()) -> string().
path(OrgName, Group) ->
    "/organizations/" ++ OrgName ++ "/groups/" ++ Group.

-spec check_for_membership(Name :: string(),
                           Type :: pushy_requestor_type(),
                           OrgName :: string(),
                           Body :: binary()) -> boolean().
%% @doc determine if a member of the group, or recurse if necessary
%%
check_for_membership(Name, Type, OrgName, Body) ->
    EJson = jiffy:decode(Body),
    Reqs = ej:get({field_for_type(Type)}, EJson),
    case lists:member(list_to_binary(Name), Reqs) of
        true ->
            true;
        false ->
            Groups = ej:get({<<"groups">>}, EJson),
            check_subgroups(Name, Type, OrgName, Groups)
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

field_for_type(user)   -> <<"users">>;
field_for_type(client) -> <<"clients">>.
