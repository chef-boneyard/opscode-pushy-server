%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-module(pushy_public_key).

-export([fetch_public_key/2]).

-spec fetch_public_key(OrgName :: binary(), Requestor :: binary()) ->
                            binary() | not_found.
%% @doc get pubkey for given requestor against given organization
get_public_key(OrgName, Requestor) ->
    request_pubkey(OrgName, Requestor).
%    try
%        request_pubkey(OrgName, Requestor)
%    catch
%        throw:_ ->
%            not_found
%    end.

request_pubkey(OrgName, Requestor) ->
    Headers = [{"Accept", "application/json"}],
    Url = api_url(OrgName, Requestor),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = check_http_response(Code, ResponseHeaders, ResponseBody),
            parse_json_response(ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

parse_json_response(Body) ->
    EJson = jiffy:decode(Body),
    case chef_authn:extract_public_or_private_key(ej:get({"public_key"}, EJson)) of
        {error, bad_key} ->
            throw({error, bad_key});
        Key when is_tuple(Key) ->
            Key
    end.

api_url(OrgName, Requestor) ->
    Hostname = "i.have.no.idea",
    Port = 9999,
    lists:flatten(io_lib:format("http://~s:~w/organizations/~w/principals/~w",
                                [Hostname, Port, OrgName, Requestor])).

check_http_response(Code, Headers, Body) ->
    case Code of
        "2" ++ _Digits ->
            ok;
        "3" ++ _Digits ->
            throw({error, {redirection, {Code, Headers, Body}}});
        "404" ->
            throw({error, {not_found, {Code, Headers, Body}}});
        "4" ++ _Digits ->
            throw({error, {client_error, {Code, Headers, Body}}});
        "5" ++ _Digits ->
            throw({error, {server_error, {Code, Headers, Body}}})
    end.
