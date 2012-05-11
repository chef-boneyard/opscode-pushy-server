%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_node_status_tracker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(state,
        {status_sock}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    ?debugVal("Starting node status tracker"),
    % expect "tcp://*:port_id"
    {ok, StatusAddress} = application:get_env(pushy, node_status_socket),
    {ok, StatusSock} = erlzmq:socket(Ctx, [pull, {active, true}]),
    ok = erlzmq:bind(StatusSock, StatusAddress),
    State = #state{status_sock = StatusSock},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({zmq, _StatusSock, Header, [rcvmore]}, State) ->
    read_message(Header),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

read_message(HeaderFrame) ->
    ?debugVal(HeaderFrame),
    receive
        {zmq, _StatusSock, BodyFrame, []} ->
            ?debugVal(BodyFrame),
            do_authenticate_message(HeaderFrame, BodyFrame),
            case catch jiffy:decode(BodyFrame) of
              {Hash} ->
                NodeName = proplists:get_value(<<"node">>, Hash),
                pushy_node_state:heartbeat(NodeName);
              {'EXIT', _Error} ->
                %% Log error and move on
                error_logger:info_msg("Status message JSON parsing failed: ~s~n", [BodyFrame])
            end
    end.

do_authenticate_message(Header, Body) ->
    SignedChecksum = signed_checksum_from_header(Header),
    % TODO - query DB for public key of each client
    {ok, PublicKey} = chef_keyring:get_key(client_public),
    Decrypted = decrypt_sig(SignedChecksum, PublicKey),
    Plain = chef_authn:hash_string(Body),
    try
        Decrypted = Plain,
        {ok}
    catch
        error:{badmatch, _} ->
            error_logger:info_msg("Status message failed verification: ~s~n", [Header]),
            {no_authn, bad_sig}
    end.

% TODO - update chef_authn to export this function
decrypt_sig(Sig, {'RSAPublicKey', _, _} = PK) ->
    try
        public_key:decrypt_public(base64:decode(Sig), PK)
    catch
        error:decrypt_failed ->
            decrypt_failed
    end.

signed_checksum_from_header(Header) ->
    HeaderParts = re:split(Header, <<":|;">>),
    {_version, _Version, _signed_checksum, SignedChecksum}
        = list_to_tuple(HeaderParts),
    SignedChecksum.
