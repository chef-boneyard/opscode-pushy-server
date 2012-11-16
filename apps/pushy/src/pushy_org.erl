%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Handle org_name to org_guid mappings by calling out to
%% opscode-account
-module(pushy_org).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_req/include/chef_rest_client.hrl").

-export([
          fetch_org_id/1,
          fetch_org_id/2
        ]).

%% @doc Fetch an org_id by querying opscode-account organizations endpoint
%% We map common error cases to specific error messages to help with debugging
%%
%%
-spec fetch_org_id(OrgName :: string()) -> binary() | not_found.
fetch_org_id(OrgName) ->
    {ok, Key} = chef_keyring:get_key(pivotal),
    User = #chef_rest_client{user_name = "pivotal",
                             private_key = Key,
                             request_source = user},
    Headers =  chef_rest_client:generate_signed_headers(User, <<"get">>,
                                                        path(OrgName), <<"">>),
    fetch_org_id(OrgName, Headers).

fetch_org_id(OrgName, Headers) ->
    Url = path(OrgName),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, "404", _ResponseHeaders, _ResponseBody} ->
            not_found;
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = check_http_response(Code, ResponseHeaders, ResponseBody),
            parse_json_response(ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

path(OrgName) ->
    {ok, ErchefHost} = application:get_env(pushy, erchef_root_url),
    list_to_binary(ErchefHost ++ "/organizations/" ++ OrgName).

%%
%% Internal functions
%%

%% @doc extract the orgid from the json structure.
%%
parse_json_response(Body) ->
    try
        EJson = jiffy:decode(Body),
        ej:get({<<"guid">>}, EJson)
    catch
        throw:{error, _} ->
            throw({error, invalid_json})
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

