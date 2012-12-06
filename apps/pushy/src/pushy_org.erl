%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Handle org_name to org_guid mappings by calling out to
%% opscode-account
-module(pushy_org).

-include_lib("eunit/include/eunit.hrl").

-export([
          fetch_org_id/1
        ]).

%% @doc Fetch an org_id by querying opscode-account organizations endpoint
%% We map common error cases to specific error messages to help with debugging
%%
%%
-spec fetch_org_id(OrgName :: string()) -> binary() | not_found.
fetch_org_id(OrgName) ->
    Path = path(OrgName),
    case pushy_http_common:fetch_authenticated(Path) of
        not_found ->
            non_found;
        ResponseBody ->
            parse_json_response(ResponseBody)
    end.

path(OrgName) ->
    "/organizations/" ++ OrgName.

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
