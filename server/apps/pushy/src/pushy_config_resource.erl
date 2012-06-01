%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
-module(pushy_config_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {
          orgname :: string(),
          organization_guid :: string() }).

init(Config) ->
    ?debugVal(Config),
    State = #config_state{},
    ?debugVal(State),
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

%is_authorized(Req, State) ->
%    OrgName =  wrq:path_info(Req, organization),
%    ?debugVal(OrgName),
%    State2 = State#config_state{orgname = OrgName},
%    {{true, foo}, Req, State2}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->

    {ok, ZeroMQListenAddress} = application:get_env(pushy, zeromq_listen_address),

    {ok, HeartbeatPort} = application:get_env(pushy, server_heartbeat_port),
    HeartbeatAddress = iolist_to_binary(io_lib:format("~s~w",[ZeroMQListenAddress,HeartbeatPort])),

    {ok, StatusPort} = application:get_env(pushy, node_status_port),
    StatusAddress = io_lib:format("~s~w",[ZeroMQListenAddress,StatusPort]),


%    {ok, PublicKeyR} = chef_keyring:get_key(server_public),
%    ?debugVal(PublicKeyR),


    PublicKey = <<"AAAAB3NzaC1kc3MAAACBAIZbwlySffbB5msSUH8JzLLXo/v03JBCWr13fVTjWYpccdbi/xL3IK/Jw8Rm3bGhnpwCAqBtsLvZ OcqXrc2XuKBYjiKWzigBMC7wC9dUDGwDl2aZ89B0jn2QPRWZuCAkxm6sKpefu++VPRRZF+iyZqFwS0wVKtl97T0gwWlzAJYpAAA AFQDIipDNo83e8RRp7Fits0DSy0DCpwAAAIB01BwXg9WSfU0mwzz/0+5Gb/TMAxfkDyucbcpJNncpRtr9Jb+9GjeZIbqkBQAqwgdbEjviRbUAuSawNSCdtnMgWD2NXkBKEde">>,

    ConfigurationStruct =
        {[{<<"type">>, <<"config">>},
                  {<<"host">>, <<"opc1.opscode.com">>},
                  {<<"push_jobs">>,
                   {[{<<"heartbeat">>,
                      {[{<<"out_addr">>, HeartbeatAddress},
                        {<<"in_addr">>, StatusAddress},
                        {<<"interval">>, 15},
                        {<<"offline_threshold">>, 3},
                        {<<"online_threshold">>, 2}
                       ]}}]}},
          {<<"public_key">>, PublicKey},
          {<<"lifetime">> ,3600}
         ]},

    ConfigurationJson = ejson:encode(ConfigurationStruct),

    ?debugVal(ConfigurationJson),

    {ConfigurationJson, Req, State}.

