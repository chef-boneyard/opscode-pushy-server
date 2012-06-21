%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General messaging utilities for ZeroMQ
-module(pushy_messaging).

-export([
         receive_message_async/2,
         send_message/2,
         send_message_multi/3,
         parse_receive_message/3
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("pushy_messaging.hrl").

%% ZeroMQ can provide frames as messages to a process. While ZeroMQ guarantees that a message is all or nothing, I don't
%% know if there is any possibility of frames of different messages being interleaved.
%%
%% List is accumulated in reverse
receive_frame_list(Socket, List) ->
    %% This clause should probably get a timeout. I b
    receive
        {zmq, Socket, Frame, [rcvmore]} ->
            receive_frame_list(Socket, [Frame | List]);
        {zmq, Socket, Frame, []} ->
            lists:reverse([Frame | List])

    end.


%%
%% Many gen_servers have a handle_info({zmq, Socket, Frame, [rcvmore]}) call
%%
receive_message_async(Socket, Frame) ->
    %% collect the full message
    receive_frame_list(Socket, [Frame]).


parse_header(Header) ->
    HeaderParts = re:split(Header, <<":|;">>),
    {_version, Version, _signed_checksum, SignedChecksum} = list_to_tuple(HeaderParts),
    {Version, SignedChecksum}.


decode_body(#message{raw=Raw} = Message) ->
      case catch jiffy:decode(Raw) of
        {Data} ->
              Message#message{body = Data};
        {'EXIT', Error} ->
              error_logger:error_msg("Status message JSON parsing failed: body=~s, error=~s~n", [Raw,Error]),
              Message#message{validated = parse_fail}
    end.

% TODO - update chef_authn to export this function
decrypt_sig(Sig, {'RSAPublicKey', _, _} = PK) ->
    try
        public_key:decrypt_public(base64:decode(Sig), PK)
    catch
        error:decrypt_failed ->
            decrypt_failed
    end.

validate_signature(#message{validated = ok_sofar,
                            signature = SignedChecksum, version = _Version,
                            raw = Raw, body = _Body} = Message) ->
    %% TODO - query DB for public key of each client
    {ok, PublicKey} = chef_keyring:get_key(client_public),

    Decrypted = decrypt_sig(SignedChecksum, PublicKey),
    case chef_authn:hash_string(Raw) of
        Decrypted -> Message;
        _Else -> Message#message{validated={fail, bad_sig}}
    end;
validate_signature(Message) -> Message.


finalize_msg(#message{validated = ok_sofar} = Message) ->
    Message#message{validated = ok}.

parse_receive_message(Socket, Frame, _SigValidator) ->
    Msg = case receive_frame_list(Socket, [Frame]) of
              [Header, Body] ->
                  {Version, SignedChecksum} = parse_header(Header),
                  #message{address = none,
                           version = Version,
                           signature = SignedChecksum,
                           raw = Body,
                           validated = ok_sofar
                          };
              [Address, Header, Body] ->
                  {Version, SignedChecksum} = parse_header(Header),
                  #message{address = Address,
                           version = Version,
                           signature = SignedChecksum,
                           raw = Body,
                           validated = ok_sofar
                          }
          end,
    Msg2 = decode_body(Msg),
    Msg3 = validate_signature(Msg2),
    finalize_msg(Msg3).






%%
%%
%%
send_message(_Socket, []) ->
    ok;
send_message(Socket, [Frame | [] ]) ->
    erlzmq:send(Socket, Frame, []);
send_message(Socket, [ Frame | FrameList]) ->
    erlzmq:send(Socket, Frame, [sndmore]),
    send_message(Socket, FrameList).

send_message_multi(Socket, AddressList, FrameList) ->
    [ send_message(Socket, [Address | FrameList] ) || Address <- AddressList ].

