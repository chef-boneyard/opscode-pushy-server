%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-module(pushy_public_key).

-export([fetch_public_key/2]).

-spec fetch_public_key(OrgName :: binary(), Requestor :: binary()) ->
                            binary() | not_found.
%% @doc get pubkey for given requestor against given organization
fetch_public_key(OrgName, Requestor) ->
    try
        request_pubkey(OrgName, Requestor)
    catch
        throw:{error, {not_found, Why}} ->
            {not_found, Why}
    end.

request_pubkey(OrgName, Requestor) ->
    Headers = [{"Accept", "application/json"},
               {"Content-Type", "application/json"},
               {"User-Agent", "opscode-pushy-server pushy pubkey"},
               {"X-Ops-UserId", ""},
               {"X-Ops-Content-Hash", ""},
               {"X-Ops-Sign", ""},
               {"X-Ops-Timestamp", ""}],
    Url = api_url(OrgName, Requestor),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = pushy_http_common:check_http_response(Code, ResponseHeaders,
                                                       ResponseBody),
            parse_json_response(ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

parse_json_response(Body) ->
    EJson = jiffy:decode(Body),
    {ej:get({"public_key"}, EJson), requestor_type(ej:get({"type"}, EJson))}.

requestor_type(<<"user">>) ->
    user;
requestor_type(<<"client">>) ->
    client;
requestor_type(_) ->
    undefined.

api_url(OrgName, Requestor) ->
    Hostname = pushy_util:get_env(erchef, hostname, "localhost", fun is_list/1),
    Port = pushy_util:get_env(erchef, port, 8000, fun is_integer/1),
    lists:flatten(io_lib:format("http://~s:~w/organizations/~s/principals/~s",
                                [Hostname, Port, OrgName, Requestor])).
