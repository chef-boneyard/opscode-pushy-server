-module(pushy_heartbeat_generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { zeromq_context,
          zeromq_publisher,
          heartbeat_interval }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, Interval} = application:get_env(pushy, heartbeat_interval),
    % expect "tcp://*:port_id"
    {ok, HeartbeatSourceSocket} = application:get_env(pushy, heartbeat_source_socket),
    {ok, ZeromqContext} = erlzmq:context(),
    {ok, ZeromqPublisher} = erlzmq:socket(ZeromqContext, pub),
    ok = erlzmq:bind(ZeromqPublisher, HeartbeatSourceSocket),
    State = #state{zeromq_context = ZeromqContext,
                   zeromq_publisher = ZeromqPublisher,
                   heartbeat_interval = Interval
                  },
    timer:apply_interval(Interval, ?MODULE, heartbeat),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, #state{zeromq_publisher=ZeromqPublisher}=State) ->
    Msg = "Hi",
    erlzmq:send(ZeromqPublisher, Msg),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

