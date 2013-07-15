-module(pushy_broker).

-behaviour(gen_server).

-include("pushy.hrl").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {frontend,
                backend_out,
                backend_in}).

start_link(PushyState) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PushyState], []).

init([#pushy_state{ctx=Ctx}]) ->
    %% Let the broker run more often
    erlang:process_flag(priority, high),
    CommandAddress = pushy_util:make_zmq_socket_addr(command_port),
    {ok, FE} = erlzmq:socket(Ctx, [router, {active, true}]),
    {ok, BEO} = erlzmq:socket(Ctx, [pull, {active, true}]),
    {ok, BEI} = erlzmq:socket(Ctx, [push, {active, false}]),
    [erlzmq:setsockopt(Sock, linger, 0) || Sock <- [FE, BEO, BEI]],
    ok = erlzmq:bind(FE, CommandAddress),
    ok = erlzmq:bind(BEO, ?PUSHY_BROKER_OUT),
    ok = erlzmq:bind(BEI, ?PUSHY_BROKER_IN),
    lager:info("~p has started.~n", [?MODULE]),
    {ok, #state{frontend=FE, backend_out=BEO, backend_in=BEI}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Incoming traffic goes to command switches
handle_info({zmq, FE, Msg, Flags}, #state{frontend=FE, backend_in=BEI}=State) ->
    case proplists:get_bool(rcvmore, Flags) of
        true ->
            erlzmq:send(BEI, Msg, [sndmore]);
        false ->
            erlzmq:send(BEI, Msg)
    end,
    {noreply, State};

%% Command switches send traffic out
handle_info({zmq, BEO, Msg, Flags}, #state{frontend=FE, backend_out=BEO}=State) ->
    case proplists:get_bool(rcvmore, Flags) of
        true ->
            erlzmq:send(FE, Msg, [sndmore]);
        false ->
            erlzmq:send(FE, Msg)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
