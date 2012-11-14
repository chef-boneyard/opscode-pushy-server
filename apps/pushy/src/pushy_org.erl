%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Handle org_name to org_guid mappings by calling out to
%% opscode-account
-module(pushy_org).

-define(POC_ORG_ID, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>).

-define(X_OPS_REQUEST_ID, "X-Ops-Request-Id").


-export([
          fetch_org_id/1,
          fetch_org_id/2
        ]).

-spec fetch_org_id(OrgName :: string()) -> binary() | not_found.
fetch_org_id(OrgName) ->
    fetch_org_id(OrgName, <<"pushyfoobar">>).

%% @doc Fetch an org_id by querying opscode-account organizations endpoint
%% We map common error cases to specific error messages to help with debugging
%%
%%
-spec fetch_org_id(OrgName :: string(),
                   RequestId :: binary() ) -> binary() | not_found.
fetch_org_id(OrgName, RequestId) ->
    FullHeaders = [{?X_OPS_REQUEST_ID, binary_to_list(RequestId)},
                   {"Accept", "application/json"}
                  ],
    {ok, RootUrl} = application:get_env(pushy, erchef_root_url),
    Url = construct_url(RootUrl, OrgName),
    case ibrowse:send_req(Url, FullHeaders, get) of
        {ok, "404", _ResponseHeaders, _ResponseBody} ->
            not_found;
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = check_http_response(Code, ResponseHeaders, ResponseBody),
            parse_json_response(ResponseBody);
        {error, Reason} ->
            throw({error, Reason})
    end.

%%
%% Internal functions
%%

-spec construct_url(RootUrl :: binary(),
                    OrgName :: binary()) -> binary().
construct_url(RootUrl, OrgName) ->
  <<RootUrl/binary, "/organizations/", OrgName/binary>>.

%% @doc extract the orgid from the json structure.
%%
parse_json_response(Body) ->
    EJson = decode(Body),
    ej:get({<<"guid">>}, EJson).


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

decode(Bin) ->
    try
        jiffy:decode(Bin)
    catch
        throw:{error, _} ->
            throw({error, invalid_json})
    end.

