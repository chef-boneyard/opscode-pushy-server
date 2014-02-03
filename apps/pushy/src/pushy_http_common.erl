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

-export([check_http_response/3,
         fetch_authenticated/1]).

fetch_authenticated(Path) ->
    {ok, Key} = chef_keyring:get_key(pivotal),
    ChefApiVersion = envy:get(pushy, chef_api_version, string),
    Headers = chef_authn:sign_request(Key, <<"pivotal">>,
                                      <<"GET">>, now, list_to_binary(Path)),
    FullHeaders = [{"Accept", "application/json"},
                   {"X-Chef-Version", ChefApiVersion} | Headers],
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

url(Path) ->
    ErchefHost = envy:get(pushy, erchef_root_url, string),
    ErchefHost ++ Path.

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
