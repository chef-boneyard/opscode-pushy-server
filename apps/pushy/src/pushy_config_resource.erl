%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_config_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         forbidden/2,
         malformed_request/2,
         service_available/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("eunit/include/eunit.hrl").

-include("pushy_sql.hrl").
-include("pushy_wm.hrl").

-define(DEFAULT_CONFIG_LIFETIME, 3600).

init(Config) ->
    pushy_wm_base:init(Config).
%%    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

service_available(Req, State) ->
    NodeName = list_to_binary(wrq:path_info(node_name, Req)),
    State1 = State#config_state{node_name = NodeName},
    {true, Req, State1}.

malformed_request(Req, State) ->
    pushy_wm_base:malformed_request(Req, State).

is_authorized(Req, State) ->
    pushy_wm_base:is_authorized(Req, State).

forbidden(Req, State) ->
    forbidden(wrq:method(Req), Req, State).
forbidden('GET', Req, #config_state{requestor_type = client} = State) ->
    case envy:get(pushy, validate_client_node_name, true, boolean) of
        true -> validate_requestor(Req, State);
        false -> {false, Req, State}
    end;
forbidden('GET', Req, State) ->
    pushy_wm_base:read_forbidden(Req, State).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, #config_state{organization_name = OrgName,
                           organization_guid = OrgGuid,
                           node_name = NodeName,
                           incarnation_id = IncarnationId,
                           requestor_key = ClientKey } = State) ->
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

    %% Implementation note: this doesn't prevent a MiM attack where a different session key
    %% is substituted. The best way to prevent this is to insure a proper SSL chain of trust
    %% from the client to the server.
    EncodedKey = public_key:encrypt_public(Key, ClientKey),
    B64EncodedKey = base64:encode(EncodedKey),
    EKeyStruct =  {[{<<"method">>, Method}, {<<"key">>, B64EncodedKey}]},

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
          {<<"encoded_session_key">>, EKeyStruct},
          {<<"lifetime">>, ConfigLifetime},
          {<<"max_message_skew">>, pushy_messaging:get_max_message_skew()},
          {<<"incarnation_id">>, IncarnationId}
         ]},
    ConfigurationJson = jiffy:encode(ConfigurationStruct),

    % ?debugVal(ConfigurationJson),

    {ConfigurationJson, Req, State}.

validate_requestor(Req, #config_state{requestor = ClientName,
                                      node_name = NodeName} = State) ->
    case ClientName =:= NodeName of
        true -> {false, Req, State};
        false ->
            Msg = <<"Client and node name must match">>,
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, [Msg]}]}), Req),
            {true, Req1, State}
    end.
