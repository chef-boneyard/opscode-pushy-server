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
    {ok, Key} = chef_keyring:get_key(pivotal),
    Path = path(OrgName, Group),
    Headers =  chef_authn:sign_request(Key, <<"">>, "pivotal",
                                       <<"GET">>, now,
                                       list_to_binary(Path)),
    FullHeaders = [{"Accept", "application/json"}|Headers],
    group_membership(Name, Type, OrgName, Group, FullHeaders).

group_membership(Name, Type, OrgName, Group, Headers) ->
    Url = url(OrgName, Group),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, "404", _ResponseHeaders, _ResponseBody} ->
            group_not_found;
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = check_http_response(Code, ResponseHeaders, ResponseBody),
            parse_json_response(Name, Type, OrgName, ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

url(OrgName, Group) ->
    {ok, ErchefHost} = application:get_env(pushy, erchef_root_url),
    ErchefHost ++ path(OrgName, Group).

path(OrgName, Group) ->
    "/organizations/" ++ OrgName ++ "/groups/" ++ Group.

%%
%% Internal functions
%%

%% @doc determine if a member of the group, or recurse if necessary
%%
parse_json_response(Name, Type, OrgName, Body) ->
    try
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
        end
    catch
        throw:{error, _} ->
            throw({error, invalid_json})
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

%% @doc Check the code of the HTTP response and throw error if non-2XX
%%
check_http_response(Code, Headers, Body) ->
    case Code of
        "2" ++ _Digits ->
            ok;
        "3" ++ _Digits ->
            throw({error, {redirection, {Code, Headers, Body}}});
        "4" ++ _Digits ->
            throw({error, {client_error, {Code, Headers, Body}}});
        "5" ++ _Digits ->
            throw({error, {server_error, {Code, Headers, Body}}})
    end.
