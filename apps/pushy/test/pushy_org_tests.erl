%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%

-module(pushy_org_tests).

-define(ROOT_URL, "https://localhost").

-include_lib("eunit/include/eunit.hrl").


-define(ORG_ID, <<"aaaaa">>).

-include_lib("eunit/include/eunit.hrl").

setup_env() ->
    ok = application:set_env(pushy, erchef_root_url, ?ROOT_URL).

make_response_body(OrgId) ->
    jiffy:encode({[{<<"guid">>, OrgId}]}).

simple_test_() ->
    %% request data
    OrgName = <<"test_org">>,
    %% response data
    OrgId = <<"test_org_id">>,
    {foreach,
     fun() ->
             setup_env(),
             meck:new([ibrowse], []),
             application:set_env(chef_authn, keyring,
                                 [{pivotal, "../test/testkey.pem"}]),
             application:set_env(chef_authn, keyring_dir, "../test"),
             application:set_env(pushy, chef_api_version, "11.0.0"),
             chef_keyring:start_link()
             %%meck:expect(chef_keyring, get_key,
             %%            fun(_) -> {ok, <<"FOO">>} end)
     end,
     fun(_) ->
              meck:unload()
     end,
    [{"Simple success test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, get) ->
                                    {ok, "200", [], make_response_body(OrgId)}
                            end),
               GotOrgId = pushy_org:fetch_org_id(OrgName),
               ?assertEqual(OrgId, GotOrgId)
               end},
     {"404 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, get) ->
                               {ok, "404", [], []}
                           end),

               ?assertEqual(not_found, pushy_org:fetch_org_id(OrgName))
      end},
     {"500 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, get) ->
                               {ok, "500", [], []}
                           end),
               ?assertThrow({error, {server_error, {"500", [], []}}}, pushy_org:fetch_org_id(OrgName))
      end},
     {"403 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, get) ->
                               {ok, "403", [], []}
                           end),
               ?assertThrow({error, {client_error, {"403",[], []}}}, pushy_org:fetch_org_id(OrgName))
      end}
    ]}.
