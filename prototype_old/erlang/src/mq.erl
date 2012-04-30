-module(mq).

-export([sink/3,
         sender/4,
         start/3,
         start/0]).

start() ->
    mq:start(<<"foo">>, <<"bar">>, "tcp://127.0.0.1:6513").

start(Identity, SinkIdentity, Endpoint) ->
    application:start(sasl),
    {ok, Ctx} = erlzmq:context(4),
    router:start_link(Ctx, Endpoint),
    timer:sleep(100),
    sink(Ctx, SinkIdentity, Endpoint),
    sink(Ctx, reverse_binary(SinkIdentity), Endpoint),
    timer:sleep(500),
    sender(Ctx, Identity, SinkIdentity, Endpoint).

sink(Ctx, Identity, Endpoint) ->
    spawn_link(fun() -> start_sink(Ctx, Identity, Endpoint) end).

sender(Ctx, Identity, SinkIdentity, Endpoint) ->
    spawn_link(fun() -> start_sender(Ctx, Identity, SinkIdentity, Endpoint) end).

start_sender(Ctx, Identity, SinkIdentity, Endpoint) ->
    io:format("Starting sender w/id: ~p~n", [Identity]),
    random:seed(erlang:now()),
    {ok, Sock} = make_socket(Ctx, dealer, Identity, false),
    ok = erlzmq:connect(Sock, Endpoint),
    do_sender(Ctx, Sock, SinkIdentity).

start_sink(Ctx, Identity, Endpoint) ->
    io:format("Starting sink w/id: ~p~n", [Identity]),
    {ok, Sock} = make_socket(Ctx, dealer, Identity, false),
    ok = erlzmq:connect(Sock, Endpoint),
    do_sink(Ctx, Sock, Identity).

do_sender(Ctx, Sock, Sink) ->
    timer:sleep(random:uniform(1000) * 5),
    Msg = list_to_binary(io_lib:format("~p", [erlang:now()])),
    mq_message:send_to_target(Sock, Sink, Msg),
    {ok, Resp} = mq_message:passive_proxied_recv(Sock),
    io:format("(sender) Reply: ~p~n", [Resp]),
    do_sender(Ctx, Sock, Sink).

do_sink(Ctx, Sock, Identity) ->
    {ok, {Sender, Req}} = mq_message:passive_proxied_recv(Sock),
    io:format("(~p) Message: ~p~n", [Identity, Req]),
    mq_message:send_to_target(Sock, Sender, Req),
    do_sink(Ctx, Sock, Identity).

make_socket(Ctx, Type, undefined, ActiveMode) ->
    {ok, Sock} = erlzmq:socket(Ctx, [Type, {active, ActiveMode}]),
    {ok, Sock};
make_socket(Ctx, Type, Identity, ActiveMode) ->
    {ok, Sock} = make_socket(Ctx, Type, undefined, ActiveMode),
    ok = erlzmq:setsockopt(Sock, identity, Identity),
    {ok, Sock}.

reverse_binary(Bin) ->
    S = size(Bin)*8,
    <<X:S/integer-little>>=Bin,
    <<X:S/integer-big>>.
