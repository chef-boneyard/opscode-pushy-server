-module(tracker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {ctx, sock}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, Ctx} = erlzmq:context(1),
    {ok, Sock} = erlzmq:socket(Ctx, [rep, {active, true}]),
    ok = erlzmq:setsockopt(Sock, identity, <<"tracker">>),
    ok = erlzmq:connect(Sock, "tcp://127.0.0.1:6513"),
    {ok, #state{ctx=Ctx, sock=Sock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("(~p) message: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
