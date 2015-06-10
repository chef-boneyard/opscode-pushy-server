%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Steven Danna <steve@chef.io>
%%
%% @end

%% @copyright Copyright 2015 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License, Version 2.0 (the "License"); you
%% may not use this file except in compliance with the License. You may obtain a copy of the
%% License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software distributed under the
%% License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
%% either express or implied. See the License for the specific language governing
%% permissions and limitations under the License.
%%

-module(pushy_principal_test).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_wm.hrl").

-define(TEST_PUBLIC_KEY, "-----BEGIN PUBLIC KEY-----\\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwb7UJ84A5W9PeGdrkcof\\nSgmlBA7JWGGZXAM/ndAceIRDkpzoxqSLTz1RY7spBcehH5cpmgeWN+8DdF8vXmER\\nl58ivKLi2rsGtvONO1f02L7/8gSbxBt9MS9JEon6n3YSizl/vG/4llfnlGSBNRQH\\nZJPlaJA9xRvlinnLi5WNjmdZHw0gucDkAc4wPRFe0WCiXkwmK4758ntpwqZNP0A1\\nmEPOE925fgw67vVOl4nd2ycBR7h1i3nBesAAdhKVR3G59zZ9wDN0xNqoaX4YidK8\\nIuWtDmsQICLMd5rwshnJ3bzlKmc+w5cYg2ZpZw35TA9zefcFKQVA0SU2bmql5h8A\\nGwIDAQAB\\n-----END PUBLIC KEY-----\\n\\n").

-define(TEST_URL_FMT, "http://localhost:8000/organizations/~s/principals/~s").

%%
%% Mock API responses
%%
%%

%% v0 API Responses
standard_principal_response_v0() ->
    principal_record("testuser", "user", true).

not_associated_principal_response_v0() ->
    principal_record("testuser", "user", false).



%% Common API Responses
not_found_principal_response(Username) ->
    lists:flatten(io_lib:format(
                    "{\"not_found\":\"principal\",\"error\":\"Cannot find principal ~s\"}",
                    [Username])).

not_found_org_response(Orgname) ->
    lists:flatten(io_lib:format(
                    "{\"not_found\":\"org\",\"error\":\"Cannot find org ~s\"}",
                    [Orgname])).

%% Helper Functions
principal_record(Username, Type, OrgMember) ->
    lists:flatten(io_lib:format("{\"name\":\"~s\",\"public_key\":\"~s\",\"type\":\"~s\",\"authz_id\":\"bfb2d2454ead3593028b467c2686df8b\",\"org_member\":~s}",
                                [Username, ?TEST_PUBLIC_KEY, Type, OrgMember])).
test_url(Orgname, Username) ->
    lists:flatten(io_lib:format(?TEST_URL_FMT, [Orgname, Username])).

fetch_principal_test_() ->
    MockedModules = [ibrowse, envy],
    {foreach,
     fun() ->
             meck:new(MockedModules, []),
             meck:expect(envy, get, [{[erchef, hostname, '_', string], "localhost"},
                                     {[erchef, port, '_', integer], 8000}]),
             meck:expect(envy, get, [{[pushy, chef_api_version, string], "12.0"}])
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"when using the v0 API, it returns a pushy_principal",
       fun() ->
               meck:expect(ibrowse, send_req, [{[test_url("anorg", "testuser"), '_', get],
                                                {ok, "200", [], standard_principal_response_v0()}}]),
               Result = pushy_principal:fetch_principal(<<"anorg">>, <<"testuser">>),
               ?assertMatch(#pushy_principal{}, Result)
       end
      },
      {"when the user isn't in the org, it returns {not_found, not_associated_with_org}",
       fun() ->
               meck:expect(ibrowse, send_req, [{[test_url("anorg", "testuser"), '_', get],
                                                {ok, "200", [], not_associated_principal_response_v0()}}]),
               Result = pushy_principal:fetch_principal(<<"anorg">>, <<"testuser">>),
               ?assertEqual(Result, {not_found, not_associated_with_org})
       end
      },
      {"when the user or client isn't found, it returns {not_found, principal}",
       fun() ->
               meck:expect(ibrowse, send_req, [{[test_url("anorg", "testuser"), '_', get],
                                                {ok, "404", [], not_found_principal_response("testuser")}}]),
               Result = pushy_principal:fetch_principal(<<"anorg">>, <<"testuser">>),
               ?assertEqual(Result, {not_found, principal})
       end
      },
      {"when the org isn't found, it returns {not_found, org}",
       fun() ->
               meck:expect(ibrowse, send_req, [{[test_url("anorg", "testuser"), '_', get],
                                                {ok, "404", [], not_found_org_response("anorg")}}]),
               Result = pushy_principal:fetch_principal(<<"anorg">>, <<"testuser">>),
               ?assertEqual(Result, {not_found, org})
       end
      }

     ]
    }.
