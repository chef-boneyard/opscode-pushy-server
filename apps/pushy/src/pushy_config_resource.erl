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

init(_Config) ->
    State = #config_state{},
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

    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    HeartbeatAddress = iolist_to_binary(
        pushy_util:make_zmq_socket_addr(Host, server_heartbeat_port, tcp)),
    CommandAddress = iolist_to_binary(
        pushy_util:make_zmq_socket_addr(Host, command_port, tcp)),

    HeartbeatInterval = pushy_util:get_env(pushy, heartbeat_interval, fun is_integer/1),

%% TODO: Figure out how to get public key out of chef_keyring in encoded form!
    {ok, PublicKeyR} = chef_keyring:get_key(pushy_pub),
    PublicKey = public_key:pem_encode(
        [public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKeyR)]),
    %% TODO: extract client name somehow
    ClientName = {<<"AAAAA">>, foo},
    SessionKey = pushy_key_manager:get_key(ClientName),
    %% TODO:
    %% The session key should be sent encrypted using the client's public key. We're
    %% skipping that for the moment, but this work is not done unless we fix this.

    ConfigurationStruct =
        {[{<<"type">>, <<"config">>},
                  {<<"host">>, iolist_to_binary(Host)},
                  {<<"push_jobs">>,
                   {[{<<"heartbeat">>,
                      {[{<<"out_addr">>, HeartbeatAddress},
                        {<<"command_addr">>, CommandAddress},
                        {<<"interval">>, HeartbeatInterval/1000},
                        {<<"offline_threshold">>, 3},
                        {<<"online_threshold">>, 2}
                       ]}}]}},
          {<<"public_key">>, PublicKey},
          {<<"session_key">>, SessionKey},
          {<<"lifetime">> ,3600}
         ]},

    ConfigurationJson = jiffy:encode(ConfigurationStruct),

    % ?debugVal(ConfigurationJson),

    {ConfigurationJson, Req, State}.

