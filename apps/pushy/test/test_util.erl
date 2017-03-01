%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Mark Anderson <mark@opscode.com>
%% @author John Keiser <john@opscode.com>
%%
%% @end


%%
%% @doc glue code for tests
%%

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

-module(test_util).

-export([start_apps/0,
         mock_chef_secrets/0
        ]).


-include_lib("eunit/include/eunit.hrl").
mock_chef_secrets() ->
    {ok, RawKey} = file:read_file(filename:join(code:priv_dir(pushy),
                                                "../test/testkey.pem")),
    meck:new(chef_secrets),
    meck:expect(chef_secrets, get, fun(<<"push-jobs-server">>, <<"sql_password">>) -> {ok, "password"};
                                      (_, _) -> {ok, RawKey}
                                   end),
    application:set_env(chef_authn, secrets_module, {chef_secrets, get,
                                                     [{pivotal, [<<"chef-server">>, <<"superuser_key">>]},
                                                      {pushy_priv, [<<"push-jobs-server">>, <<"pushy_priv_key">>]},
                                                      {pushy_pub, [<<"push-jobs-server">>, <<"pushy_pub_key">>]}]}).


start_apps() ->
    application:start(gproc),
    application:start(folsom).
