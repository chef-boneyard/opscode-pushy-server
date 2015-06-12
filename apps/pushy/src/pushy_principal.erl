%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>

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
-module(pushy_principal).
-compile([{parse_transform, lager_transform}]).
-include("pushy_wm.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([fetch_principals/2]).

-spec fetch_principals(OrgName :: binary(),
                      Requestor :: binary()) -> [#pushy_principal{}] | {not_found | conn_failed, term()}.
fetch_principals(OrgName, Requestor) ->
    try
        request_principals(OrgName, Requestor)
    catch
        throw:{error, {not_found, Why}} ->
            {not_found, Why};
        throw:{error, {conn_failed, Why}} ->
            {conn_failed, Why}
    end.

-spec request_principals(OrgName :: binary(),
                        Requestor :: binary()) -> [#pushy_principal{}].
request_principals(OrgName, Requestor) ->
    Path = api_path(OrgName, Requestor),
    Response = pushy_http_common:fetch_unauthenticated(Path),
    case parse_principal_response(Response) of
        {error, Reason} ->
            lager:error("Error requesting principal ~p: ~p~n", [Requestor, Reason]),
            throw({error, Reason});
        [#pushy_principal{}|_Rest] = Principals ->
            Principals
    end.

-spec parse_principal_response(#chef_api_response{} | {error, any()}) -> [#pushy_principal{}] | {error, any()}.
parse_principal_response({error, Reason}) ->
    {error, Reason};
parse_principal_response(#chef_api_response{response_code = "404",
                                            response_body = Body
                                           }) ->
    Reason = case ej:get({"not_found"}, Body) of
                 <<"org">> -> org;
                 <<"principal">> -> principal
             end,
    {error, {not_found, Reason}};
parse_principal_response(#chef_api_response{response_api_version = 1,
                                            response_body = EJson} = Resp) ->
    ok = pushy_http_common:check_response(Resp),
    Principals = ej:get({"principals"}, EJson),
    Principals1 = [parse_ejson_response(P) || P <- Principals],
    filter_not_associated(Principals1);
parse_principal_response(#chef_api_response{response_api_version = 0,
                                            response_body=EJson} = Resp) ->
    ok = pushy_http_common:check_response(Resp),
    filter_not_associated([parse_ejson_response(EJson)]).


%% Filter out any not_associated_with_org error tuples from our list of principals.  If
%% filtering results in an empty list, return an error.
filter_not_associated(Principals) ->
    filter_not_associated(Principals, []).

filter_not_associated([], []) ->
    {error, {not_found, not_associated_with_org}};
filter_not_associated([], Accum) ->
    Accum;
filter_not_associated([{error, {not_found, not_associated_with_org}}|Rest], Accum) ->
    filter_not_associated(Rest, Accum);
filter_not_associated([#pushy_principal{} = Principal|Rest], Accum) ->
    filter_not_associated(Rest, [Principal|Accum]).

-spec parse_ejson_response(EJson :: ejson_term()) -> #pushy_principal{} | {error, {not_found, not_associated_with_org}}.
parse_ejson_response(EJson) ->
    case ej:get({"org_member"}, EJson) of
        true ->
            #pushy_principal{requestor_key = ej:get({"public_key"}, EJson),
                             requestor_type = requestor_type(ej:get({"type"}, EJson)),
                             requestor_id = ej:get({"authz_id"}, EJson)};
        _ ->
            {error, {not_found, not_associated_with_org}}
    end.

requestor_type(<<"user">>)   -> user;
requestor_type(<<"client">>) -> client.

-spec api_path(OrgName :: binary(), Requestor :: binary()) -> list().
api_path(OrgName, Requestor) ->
    lists:flatten(io_lib:format("/organizations/~s/principals/~s",
                                [OrgName, Requestor])).
