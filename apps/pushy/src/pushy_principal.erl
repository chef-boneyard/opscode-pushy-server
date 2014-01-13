%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-module(pushy_principal).

-include("pushy_wm.hrl").

-export([fetch_principal/2]).

-spec fetch_principal(OrgName :: binary(),
                      Requestor :: binary()) -> #pushy_principal{} | {not_found | conn_failed, term()}.
fetch_principal(OrgName, Requestor) ->
    try
        request_principal(OrgName, Requestor)
    catch
        throw:{error, {not_found, Why}} ->
            {not_found, Why};
        throw:{error, {conn_failed, Why}} ->
            {conn_failed, Why}
    end.

-spec request_principal(OrgName :: binary(),
                        Requestor :: binary()) -> #pushy_principal{}.
request_principal(OrgName, Requestor) ->
    ChefVersion = envy:get(pushy, chef_version, string),
    Headers = [{"Accept", "application/json"},
               {"Content-Type", "application/json"},
               {"User-Agent", "opscode-pushy-server pushy pubkey"},
               {"X-Ops-UserId", ""},
               {"X-Ops-Content-Hash", ""},
               {"X-Ops-Sign", ""},
               {"X-Ops-Timestamp", ""},
               {"X-Chef-Version", ChefVersion}],
    Url = api_url(OrgName, Requestor),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = pushy_http_common:check_http_response(Code, ResponseHeaders,
                                                       ResponseBody),
            parse_json_response(ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

-spec parse_json_response(Body :: binary()) -> #pushy_principal{}.
parse_json_response(Body) ->
    EJson = jiffy:decode(Body),
    case ej:get({"org_member"}, EJson) of
        true ->
            #pushy_principal{requestor_key = ej:get({"public_key"}, EJson),
                             requestor_type = requestor_type(ej:get({"type"}, EJson)),
                             requestor_id = ej:get({"authz_id"}, EJson)};
        _ ->
            throw({error, {not_found, not_associated_with_org}})
    end.

requestor_type(<<"user">>)   -> user;
requestor_type(<<"client">>) -> client.

-spec api_url(OrgName :: binary(), Requestor :: binary()) -> list().
api_url(OrgName, Requestor) ->
    Hostname = envy:get(erchef, hostname, "localhost", string),
    Port = envy:get(erchef, port, 8000, integer),
    lists:flatten(io_lib:format("http://~s:~w/organizations/~s/principals/~s",
                                [Hostname, Port, OrgName, Requestor])).
