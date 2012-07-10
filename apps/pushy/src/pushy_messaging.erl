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


         parse_receive_message/3,
         send_message_record/2,


         build_outgoing_message/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlzmq/include/erlzmq.hrl").

-include("pushy_messaging.hrl").
-include("pushy_metrics.hrl").




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
%% Many gen_servers have a handle_info({zmq, Socket, Frame, [rcvmore]}) call which
%% immediately get a set of frames from zmq.
%%
receive_message_async(Socket, Frame) ->
    %% collect the full message
    receive_frame_list(Socket, [Frame]).


-spec parse_receive_message(Socket::erlzmq_socket_type(), Frame::binary(), SigValidator::fun()) ->
                                   #message{}.
parse_receive_message(Socket, Frame, _SigValidator) ->
    Msg1 = case receive_frame_list(Socket, [Frame]) of
              [Header, Body] -> build_message_record(none, Header, Body);
              [Address, Header, Body] -> build_message_record(Address, Header, Body)
          end,
    Msg2 = parse_body(Msg1),
%    lager:error("Processed msg (2) ~p ~p", [Msg2#message.validated, Msg2#message.body]),
    Msg3 = validate_signature(Msg2),
%    lager:error("Processed msg (3) ~p ~p", [Msg3#message.validated, Msg3#message.body]),
    finalize_msg(Msg3).


%%
%% Build a message record for the various parse and validation stages to process
%%
-spec build_message_record(Address::binary() | none, Header::binary(), Body::binary()) -> #message{}.
build_message_record(Address, Header, Body) ->
    Id = make_ref(),
    {Version, SignedChecksum} = parse_header(Header),
    lager:debug("Received msg ~w (~w:~w:~w)",[Id, len_h(Address), len_h(Header), len_h(Body)]),
    #message{validated = ok_sofar,
             id = Id,
             address = Address,
             version = Version,
             signature = SignedChecksum,
             raw = Body
            }.

len_h(none) -> 0;
len_h(B) when is_binary(B) -> erlang:byte_size(B).


%%
%%
%%
parse_header(Header) ->
    HeaderParts = re:split(Header, <<":|;">>),
    {_version, Version, _signed_checksum, SignedChecksum} = list_to_tuple(HeaderParts),
    {Version, SignedChecksum}.

%%
%% Parse the json body of the message
%%
parse_body(#message{validated = ok_sofar,
                    raw=Raw} = Message) ->
    try jiffy:decode(Raw) of
         {Data} ->
            Message#message{body = Data}
    catch
        exit:_Error ->
            lager:error("JSON parsing failed with error: ~s", [error]),
            Message#message{validated = parse_fail}
    end;
parse_body(Message) -> Message.

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
%    Decrypted = ?TIME_IT(?MODULE, decrypt_sig, (SignedChecksum, PublicKey)),
    Decrypted = decrypt_sig(SignedChecksum, PublicKey),
    case chef_authn:hash_string(Raw) of
        Decrypted -> Message;
        _Else ->
            case pushy_util:get_env(pushy, ignore_signature_check, false, fun is_boolean/1) of
                true -> ok;
                false ->
                    lager:error("Validation failed ~s ~s~n", [Decrypted, _Else]),
                    Message#message{validated={fail, bad_sig}}
            end
        end;
validate_signature(Message) -> Message.


finalize_msg(#message{validated = ok_sofar} = Message) ->
    Message#message{validated = ok}.



%%
%%
%%


-spec send_message_record(Socket::erlzmq_socket_type(), Message::#message{} ) -> ok.
send_message_record(Socket, #message{address=none, header=Header, raw=Body}) ->
    send_message(Socket, [Header, Body]);
send_message_record(Socket, #message{address=Address, header=Header, raw=Body}) ->
    send_message(Socket, [Address, Header, Body]).

%%
%% build_message_record
%%
-spec build_outgoing_message(Version::atom(), JsonBody::json_term()) -> #message{}.
build_outgoing_message(version_1, JsonBody) ->
    Msg0 = #message{validated = json_only,
                    id = make_ref(),
                    address = none,
                    header = none,
                    raw = none,
                    version = none,
                    signature = none,
                    body = JsonBody},
    Msg1 = encode_json(Msg0),
    sign_msg(Msg1).


encode_json(#message{validated=json_only, body=Body}=Msg) ->
    try
        Raw = jiffy:encode(Body),
        Msg#message{validated = raw_encoded,
                    raw = Raw}
    catch
        error:Error ->
            Msg#message{validated = Error}
    end.

sign_msg(Msg) ->
    Msg.



send_message(_Socket, []) ->
    ok;
send_message(Socket, [Frame | [] ]) ->
    erlzmq:send(Socket, Frame, []);
send_message(Socket, [ Frame | FrameList]) ->
    erlzmq:send(Socket, Frame, [sndmore]),
    send_message(Socket, FrameList).

send_message_multi(Socket, AddressList, FrameList) ->
    [ send_message(Socket, [Address | FrameList] ) || Address <- AddressList ].
