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
         service_available/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

-define(DEFAULT_CONFIG_LIFETIME, 3600).

init(_Config) ->
    State = #config_state{},
    {ok, State}.
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

service_available(Req, State) ->
    NodeName = wrq:path_info(node_name, Req),
    OrgName = wrq:path_info(organization_id, Req),
    OrgGuid = ?POC_ORG_ID,
    State1 = State#config_state{orgname = OrgName, organization_guid = OrgGuid,
                                nodename = NodeName},
    {true, Req, State1}.

% TODO: Whenever the client/tests handle authentication correctly (per OC-4790),
% uncomment these, as well as removing the above service_available function:

%service_available(Req, State) ->
%    NodeName = wrq:path_info(node_name, Req),
%    State1 = State#config_state{nodename = NodeName},
%    {true, Req, State1}.

%malformed_request(Req, State) ->
%    pushy_wm_base:malformed_request(Req, State).

%is_authorized(Req, State) ->
%    pushy_wm_base:is_authorized(Req, State).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, #config_state{orgname = OrgName, organization_guid = OrgGuid, nodename = NodeName} = State) ->

    Host = envy:get(pushy, server_name, string),
    ConfigLifetime = envy:get(pushy, config_lifetime, ?DEFAULT_CONFIG_LIFETIME, integer),
    HeartbeatAddress = iolist_to_binary(
        pushy_util:make_zmq_socket_addr(Host, server_heartbeat_port, tcp)),
    CommandAddress = iolist_to_binary(
        pushy_util:make_zmq_socket_addr(Host, command_port, tcp)),

    HeartbeatInterval = envy:get(pushy, heartbeat_interval, integer),

    %% TODO: OC-4352
    %% Perhaps chef_keyring should be enhanced with key format management functions.
    {ok, PublicKeyR} = chef_keyring:get_key(pushy_pub),
    PublicKey = public_key:pem_encode(
        [public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKeyR)]),

    ClientName = {OrgGuid, iolist_to_binary(NodeName)},
    {Method,Key} = pushy_key_manager:get_key(ClientName),

    %% TODO: OC-4204
    %% The session key should be sent encrypted using the client's public key. We're
    %% skipping that for the moment, but this work is not done unless we fix this.
    %% This needs the client key fetch work to be done first though...
    KeyStruct =  {[{<<"method">>, Method}, {<<"key">>, base64:encode(Key) }]},

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
          {<<"node">>, NodeName},
          {<<"organization">>, OrgName},
          {<<"public_key">>, PublicKey},
          {<<"session_key">>, KeyStruct},
          {<<"lifetime">> ,ConfigLifetime}
         ]},
    ?debugVal(ConfigurationStruct),
    ConfigurationJson = jiffy:encode(ConfigurationStruct),

    % ?debugVal(ConfigurationJson),

    {ConfigurationJson, Req, State}.

