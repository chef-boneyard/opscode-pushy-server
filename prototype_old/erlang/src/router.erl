-module(router).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {sock}).
-define(zmq_msg, {sender,
                  target,
                  body}).

start_link(Ctx, Endpoint) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Ctx, Endpoint], []).

init([Ctx, Endpoint]) ->
    {ok, Sock} = erlzmq:socket(Ctx, [router, {active, true}]),
    ok = erlzmq:bind(Sock, Endpoint),
    {ok, ready, #state{sock=Sock}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.

handle_info({zmq, _Sock, Sender, [rcvmore]}, ready, State) ->
    read_message(Sender),
    {next_state, ready, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
read_message(Sender) ->
    receive
        {zmq, _Sock, Target, [rcvmore]} ->
            read_message(Sender, Target, [])
    end.

read_message(Sender, Target, Body) ->
    receive
        {zmq, _Sock, BodyPart, [rcvmore]} ->
            read_message(Sender, Target, [BodyPart|Body]);
        {zmq, Sock, BodyPart, []} ->
            route_message(Sock, Sender, Target, list_to_binary(lists:reverse([BodyPart|Body])))
    end.

route_message(Sock, Sender, Target, Message) ->
    io:format("From: ~p~nTo: ~p~nBody: ~p~n", [Sender, Target, Message]),
    ok = erlzmq:send(Sock, Target, [sndmore]),
    ok = erlzmq:send(Sock, Sender, [sndmore]),
    ok = erlzmq:send(Sock, Message, []).
