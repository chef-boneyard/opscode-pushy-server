%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_heartbeat_generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         stats/0,
         heartbeat/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state,
        {heartbeat_sock,
         heartbeat_interval,
         beat_count,
         private_key}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ctx) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ctx], []).

stats() ->
    gen_server:call(?SERVER, stats, infinity).

heartbeat() ->
    gen_server:cast(?SERVER, heartbeat).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ctx]) ->
    %error_logger:info_msg("Starting heartbeat generator."),
    {ok, Interval} = application:get_env(pushy, heartbeat_interval),
    % expect "tcp://*:port_id"
    {ok, HeartbeatAddress} = application:get_env(pushy, server_heartbeat_socket),
    {ok, HeartbeatSock} = erlzmq:socket(Ctx, pub),
    {ok, PrivateKey} = chef_keyring:get_key(server_private),
    ok = erlzmq:bind(HeartbeatSock, HeartbeatAddress),
    State = #state{heartbeat_sock = HeartbeatSock,
                   heartbeat_interval = Interval,
                   beat_count = 0,
                   private_key = PrivateKey
                  },
    timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, State}.

handle_call(stats, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(heartbeat,
    #state{heartbeat_sock=HeartbeatSock, beat_count=Count, private_key=PrivateKey}=State) ->
    {ok, Hostname} = inet:gethostname(),
    Msg = {[{server, list_to_binary(Hostname)},
            {timestamp, list_to_binary(httpd_util:rfc1123_date())},
            {type, heartbeat}]},
    % JSON encode message
    BodyFrame = jiffy:encode(Msg),

    % Send Header (including signed checksum)
    HeaderParts = sign_message(PrivateKey, BodyFrame),
    Headers = [join_bins(tuple_to_list(Part), <<":">>) || Part <- HeaderParts],
    HeaderFrame = join_bins(Headers, <<";">>),
    erlzmq:send(HeartbeatSock, HeaderFrame, [sndmore]),
    %?debugVal(HeaderFrame),
    % Send Body
    erlzmq:send(HeartbeatSock, BodyFrame),
    %?debugVal(BodyFrame),
    %error_logger:info_msg("Heartbeat sent: header=~s,body=~s~n",[HeaderFrame, BodyFrame]),
    {noreply, State#state{beat_count=Count+1}};
handle_cast(_Msg, State) ->
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

%% @doc Sign a heartbeat message so it can be published to the clients
%%
%% Returns a list of header tuples that should be included in the
%% final ZMQ message.
%%
sign_message(PrivateKey, Body) ->
    HashedBody = chef_authn:hash_string(Body),
    SignedChecksum = base64:encode(public_key:encrypt_private(HashedBody, PrivateKey)),
    [{<<"Version">>, <<"1.0">>},
     {<<"SignedChecksum">>, SignedChecksum}].

join_bins([], _Sep) ->
    <<>>;
join_bins(Bins, Sep) when is_binary(Sep) ->
    join_bins(Bins, Sep, []).

join_bins([B], _Sep, Acc) ->
    iolist_to_binary(lists:reverse([B|Acc]));
join_bins([B|Rest], Sep, Acc) ->
    join_bins(Rest, Sep, [Sep, B | Acc]).
