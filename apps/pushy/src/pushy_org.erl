%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>

%% @doc Handle org_name to org_guid mappings by calling out to
%% opscode-account
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
            not_found;
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
