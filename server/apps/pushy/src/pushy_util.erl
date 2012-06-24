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
         signed_header_from_message/2
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
