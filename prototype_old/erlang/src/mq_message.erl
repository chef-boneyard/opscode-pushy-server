-module(mq_message).

-export([passive_proxied_recv/1,
         proxy_to_target/4,
         send_to_target/3]).

proxy_to_target(Sock, ProxiedSender, Target, Message) ->
    ok = erlzmq:send(Sock, Target, [sndmore]),
    ok = erlzmq:send(Sock, ProxiedSender, [sndmore]),
    ok = erlzmq:send(Sock, Message, []).

send_to_target(Sock, Target, Message) ->
    ok = erlzmq:send(Sock, Target, [sndmore]),
    ok = erlzmq:send(Sock, Message, []).

passive_proxied_recv(Sock) ->
    case erlzmq:recv(Sock) of
        {ok, Sender} ->
            case erlzmq:getsockopt(Sock, rcvmore) of
                {ok, 1} ->
                    passive_proxied_recv(Sock, Sender, []);
                {ok, 0} ->
                    {{error, bad_message}, Sender}
            end;
        Error ->
            Error
    end.

passive_proxied_recv(Sock, Sender, Body) ->
    case erlzmq:recv(Sock) of
        {ok, Part} ->
            case erlzmq:getsockopt(Sock, rcvmore) of
                {ok, 1} ->
                    passive_proxied_recv(Sock, Sender, [Part|Body]);
                {ok, 0} ->
                    {ok, {Sender, list_to_binary(lists:reverse([Part|Body]))}}
            end;
        Error ->
            Error
    end.
