%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General messaging utilities for ZeroMQ
-module(pushy_messaging).

-export([
         receive_message_async/2,
         send_message/2,
         send_message_multi/3
        ]).

-include_lib("eunit/include/eunit.hrl").

%% -record(message,
%%         {address :: binary() | none,
%%          version :: binary(),
%%          signature :: binary() | none,
%%          raw  :: binary(), 
%%          body :: any() % Get a viable json type here
%%         }).


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

