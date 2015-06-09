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
-module(pushy_http_common).
-include("pushy_wm.hrl").

-export([check_response/1,
         fetch_unauthenticated/1,
         fetch_authenticated/1
        ]).

-define(PREFERED_API_VERSION, "1").

fetch_authenticated(Path) ->
    {ok, Key} = chef_keyring:get_key(pivotal),
    Headers = chef_authn:sign_request(Key, <<"pivotal">>,
                                      <<"GET">>, now, list_to_binary(Path)),
    FullHeaders = [{"Accept", "application/json"},
                   {"X-Ops-Server-API-Version", ?PREFERED_API_VERSION},
                   {"X-Chef-Version", chef_version()} | Headers],
    fetch_authenticated(Path, FullHeaders).

fetch_authenticated(Path, Headers) ->
    Url = url(Path),
    case ibrowse:send_req(Url, Headers, get) of
        {ok, "404", _ResponseHeaders, _ResponseBody} ->
            not_found;
        {ok, Code, ResponseHeaders, ResponseBody} ->
            ok = check_http_response(Code, ResponseHeaders, ResponseBody),
            ResponseBody;
        {error, Reason} ->
            throw({error, Reason})
    end.

fetch_unauthenticated(Path) ->
    Headers = unauthenticated_headers(),
    Url = unauthenticated_url(Path),
    parse_chef_api_response(ibrowse:send_req(Url, Headers, get)).

-spec parse_chef_api_response(ibrowse_response()) -> chef_api_response() | {error, any()}.
parse_chef_api_response({ok, Code, Headers, ResponseBody}) ->
    EJBody = jiffy:decode(ResponseBody),
    ResponseVersion = response_version_from_headers(Headers),
    #chef_api_response{ response_api_version = ResponseVersion,
                        response_code = Code,
                        response_headers = Headers,
                        response_body = EJBody};
parse_chef_api_response({error, Reason}) ->
    {error, Reason}.


-spec response_version_from_headers(list()) -> 0 | 1.
response_version_from_headers(Headers) ->
    case lists:keyfind("X-Ops-Server-API-Version", 1, Headers) of
        {"X-Ops-Server-API-Version", VersionJSON} ->
            EJ = jiffy:decode(VersionJSON),
            %% There are more fields here, but for now we only need this as we won't be retrying
            Version = ej:get({"response_version"}, EJ),
            binary_to_integer(Version);
        false ->
            0
    end.

base_headers() ->
    [{"Accept", "application/json"},
     {"Content-Type", "application/json"},
     {"X-Ops-Server-API-Version", ?PREFERED_API_VERSION},
     {"User-Agent", "opscode-pushy-server pushy pubkey"}].

unauthenticated_headers() ->
    [ {"X-Ops-UserId", ""},
      {"X-Ops-Content-Hash", ""},
      {"X-Ops-Sign", ""},
      {"X-Ops-Timestamp", ""},
      {"X-Chef-Version", chef_version()} | base_headers()].

chef_version() ->
    envy:get(pushy, chef_api_version, string).

url(Path) ->
    ErchefHost = envy:get(pushy, erchef_root_url, string),
    ErchefHost ++ Path.

unauthenticated_url(Path) ->
    %% Send all unauthenticated requests directly to erchef
    %% as these endpoints may not be exposed via the LB
    Hostname = envy:get(erchef, hostname, "localhost", string),
    Port = envy:get(erchef, port, 8000, integer),
    lists:flatten(io_lib:format("http://~s:~w~s", [Hostname, Port, Path])).

check_response(#chef_api_response{response_code = Code,
                                  response_body = Body,
                                  response_headers = Headers}) ->
    check_http_response(Code, Headers, Body).

check_http_response(Code, Headers, Body) ->
    case Code of
        "200" ->
            ok;
        "2" ++ _Digits ->
            throw({error, {unexpected, {Code, Headers, Body}}});
        "3" ++ _Digits ->
            throw({error, {redirection, {Code, Headers, Body}}});
        "404" ->
            throw({error, {not_found, {Code, Headers, Body}}});
        "4" ++ _Digits ->
            throw({error, {client_error, {Code, Headers, Body}}});
        "5" ++ _Digits ->
            throw({error, {server_error, {Code, Headers, Body}}})
    end.
