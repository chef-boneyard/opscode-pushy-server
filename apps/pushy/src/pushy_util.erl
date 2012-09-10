%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General utility module for common functions that operate on
-module(pushy_util).

-export([
         make_zmq_socket_addr/1,
         make_zmq_socket_addr/2,
         make_zmq_socket_addr/3,
         get_env/3,
         get_env/4,
         read_body/0,
         do_authenticate_message/2,
         signed_header_from_message/2,
         rand_bytes/1,
         guid_v4/0,
         gen_req_id_using_rand/2,
         gproc_match_head/3
        ]).

-include_lib("eunit/include/eunit.hrl").

make_zmq_socket_addr(Port) ->
    Host = get_env(pushy, zeromq_listen_address, fun is_list/1),
    make_zmq_socket_addr(Host, Port).

make_zmq_socket_addr(Host, PortName, tcp) ->
    ProtoHost = io_lib:format("tcp://~s", [Host]),
    make_zmq_socket_addr(ProtoHost, PortName).


make_zmq_socket_addr(Host, PortName) when is_atom(PortName) ->
    Port = get_env(pushy, PortName, fun is_integer/1),
    make_zmq_socket_addr(Host, Port);
make_zmq_socket_addr(Host, Port) when is_integer(Port) ->
    lists:flatten(io_lib:format("~s:~w",[Host,Port])).

get_env(Section, Item, any) ->
    get_env(Section, Item, fun(_) -> true end);
get_env(Section, Item, TypeCheck) ->
    case application:get_env(Section, Item) of
        {ok, Value} ->
            case TypeCheck(Value) of
                true -> Value;
                Error ->
                    lager:error("Bad typecheck for config item for ~p ~p (~p(~p) -> ~p)",
                                           [Section, Item, TypeCheck, Value, Error]),
                    error(config_bad_item)
            end;
        undefined ->
            lager:error("Bad config item for ~p ~p ", [Section, Item]),
            error(config_missing_item)
    end.


get_env(Section, Item, Default, any) ->
    get_env(Section, Item, Default, fun(_) -> true end);
get_env(Section, Item, Default, TypeCheck) ->
    case application:get_env(Section, Item) of
        {ok, Value} ->
            case TypeCheck(Value) of
                true -> Value;
                Error ->
                    lager:error("Bad typecheck for config item for ~p ~p (~p(~p) -> ~p)",
                                           [Section, Item, TypeCheck, Value, Error]),
                    error(config_bad_item)
            end;
        undefined ->
            Default
    end.

%%
%% Factor out common packet handling methods
%%

read_body() ->
    receive
        {zmq, _Sock, BodyFrame, []} ->
            BodyFrame
    end.

%% DECODE/DECRYPT

do_authenticate_message(Header, Body) ->
    SignedChecksum = signed_checksum_from_header(Header),
    % TODO - query DB for public key of each client
    {ok, PublicKey} = chef_keyring:get_key(client_public),
    Decrypted = decrypt_sig(SignedChecksum, PublicKey),
    Plain = chef_authn:hash_string(Body),
    case Decrypted of
        Plain -> ok;
        _Else ->
            case get_env(pushy, ignore_signature_check, false, fun is_boolean/1) of
                true -> ok;
                false -> {no_authn, bad_sig}
            end
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

%% ENCODE/ENCRYPT
signed_header_from_message(PrivateKey, Body) ->
    HashedBody = chef_authn:hash_string(Body),
    SignedChecksum = base64:encode(public_key:encrypt_private(HashedBody, PrivateKey)),
    Headers = [join_bins(tuple_to_list(Part), <<":">>) || Part <- [{<<"Version">>, <<"1.0">>},
                    {<<"SignedChecksum">>, SignedChecksum}]],
    join_bins(Headers, <<";">>).

join_bins([], _Sep) ->
    <<>>;
join_bins(Bins, Sep) when is_binary(Sep) ->
    join_bins(Bins, Sep, []).

join_bins([B], _Sep, Acc) ->
    iolist_to_binary(lists:reverse([B|Acc]));
join_bins([B|Rest], Sep, Acc) ->
    join_bins(Rest, Sep, [Sep, B | Acc]).


%%% R15 introduces strong_rand_bytes, which is preferable, but we still need to work on older versions.
-spec rand_bytes(non_neg_integer()) -> binary().
rand_bytes(NBytes) ->
    case lists:member(strong_rand_bytes, crypto:info()) of
        false -> crypto:rand_bytes(NBytes);
        true -> crypto:strong_rand_bytes(NBytes)
    end.

% RFC4122 V4 GUID
% Version 4 UUIDs have the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
% where x is any hexadecimal digit and
% y is one of 8, 9, A, or B
-spec guid_v4() -> string().
guid_v4() ->
    <<TL:32, TM:16, TH:12, CS:14, N:48, _:6>> = rand_bytes(16),
    THV = TH bor (4 bsl 12),
    CSV = CS bor (2 bsl 14), % see section 4.4 of RFV (set high order bits to '10XX_XXXX_XXXX_XXXX')
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [TL, TM, THV, CSV, N])).

-spec gen_req_id_using_rand(string() | binary(), non_neg_integer()) -> binary().
gen_req_id_using_rand(Prefix, NBytes) ->
    RandBytes = rand_bytes(NBytes),
    NBits = NBytes*8,
    <<RandValue:NBits, _/binary>> = RandBytes,
    iolist_to_binary([Prefix, integer_to_list(RandValue, 16)]).

%% MatchHead for gproc
-spec gproc_match_head(atom(), atom(), any()) -> {{atom(), atom(), any()}, '_', '_'}.
gproc_match_head(Type, Scope, Key) ->
    {{Type, Scope, Key}, '_', '_'}.


