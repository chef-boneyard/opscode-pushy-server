%%%-------------------------------------------------------------------
%%% @author James Casey
%%% @copyright 2012 Opscode, Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pushy_process_monitor).

-behaviour(gen_server).

%% API
-export([start_link/3,
         measure/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type process() :: atom() | pid().
-record(state, {name :: binary(),
                process :: process(),
                interval :: integer()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Process, Interval) ->
    gen_server:start_link(?MODULE, [Name, Process, Interval], []).

measure(Pid) ->
  gen_server:cast(Pid, measure).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Process, Interval]) ->
    {ok, _Timer} = timer:send_after(Interval, start_measure),
    {ok, #state{name = Name,
                process = Process,
                interval = Interval}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(measure, #state{name = Name,
                            process = Process} = State) ->
  case mbox_stats(Process) of
    undefined ->
      lager:warning("Process undefined : ~p", [Process]),
      ignore;
    Len when is_integer(Len) ->
      send_metric(Name, Len)
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(start_measure, #state{process = Process,
                                  interval = Interval} = State) ->
    lager:info("Starting Process Monitor for ~p", [Process]),
    timer:apply_interval(Interval, ?MODULE, measure, [self()]),
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("handle_info: [~s] unhandled message ~w:", [State, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


mbox_stats(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            undefined;
        Pid ->
            mbox_stats(Pid)
    end;
mbox_stats(Pid) ->
    {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
    Len.

send_metric(Name, Len) ->
   folsom_metrics:notify(metric_for_name(Name), Len, gauge).

metric_for_name(Name) ->
  <<"process.", Name/binary, ".message_queue_len">>.
