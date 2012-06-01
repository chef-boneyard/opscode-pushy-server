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

    Host = "tcp://33.33.33.10:",
    HeartbeatAddress = iolist_to_binary(pushy_util:make_zmq_socket_addr(Host, server_heartbeat_port)),
    StatusAddress = iolist_to_binary(pushy_util:make_zmq_socket_addr(Host, node_status_port)),

%% TODO: Figure out how to get public key out of chef_keyring in encoded form!
%    {ok, PublicKeyR} = chef_keyring:get_key(server_public),
%    ?debugVal(PublicKeyR),
%    EncKey = public_key:pem_encode([public_key:pem_entr),
%    ?debugVal(EncKey),


    PublicKey =
        <<"-----BEGIN PUBLIC KEY-----\n"
          "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwLm8nRqzKICAP9LMNtpF\n"
          "5LAFbOjCRvceprp+Gs6M1xU3csU0S16ZpBUuXslIcJyOWWZtfyqW2GPb4+zNlbAQ\n"
          "lsg26ngpw+XT9e4XH2ufp8xkQpwOBiA+1uo54uUj3PSxb1fySvPzpGzuhxOuGgBP\n"
          "JUrE7EE0eQFl3lTFDz9EtRgYc9x9kmbf0CfxX+YdMLCTsWJKC+BmzYARtFtf6rJc\n"
          "mpN0EF29kBunF2AckjF3EYm6H351BnJtCZMZn2vqBF1UPt4bdnuloUdPW2xOf0Zm\n"
          "LiyfwjODDOinSTpE5E8WfU1efXST7GvYZ1CmWS41c1UZBsqjLUDDlQNM8jWmpHqb\n"
          "4QIDAQAB\n"
          "-----END PUBLIC KEY-----">>,

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

