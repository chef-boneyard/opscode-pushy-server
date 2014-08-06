%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(pushy_org_events).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_event.hrl").

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1]).
-export([get_events/2]).
% Used solely for testing
-export([get_expiration/0]).
-export([clear_events/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          next_event_id   :: integer(),
          rev_events      :: [#event{}],
          subscribers     :: [pid()]
         }).

start_link(Org) ->
    gen_server:start_link(?MODULE, [Org], []).

get_events(Org, LastEventId) ->
    Pid = pushy_org_events_sup:get_or_create_process(Org),
    gen_server:call(Pid, {get_events, LastEventId}).

clear_events(Org) ->
    Pid = pushy_org_events_sup:get_or_create_process(Org),
    gen_server:call(Pid, clear_events).

init(_Org) ->
    State = #state{
           next_event_id = 1,
           rev_events = [],
           subscribers = []
        },
    {ok, State}.

handle_call({get_events, LastEventID}, {Pid, _}, State) ->
    State1 = expire_old_events(State),
    Response = build_event_response(LastEventID, State1),
    State2 = add_subscriber(Pid, State),
    {reply, Response, State2};
handle_call(clear_events, _From, State) ->
    {reply, ok, State#state{rev_events = []}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({job_ev, JobId, Event}, State) ->
    State1 = handle_incoming_event(Event, JobId, State),
    {noreply, State1};
handle_info(ping, State) ->
    post_to_subscribers(State, ping),
    {noreply, State};
handle_info(done, State) ->
    % XXX Synthesize job_complete if we never saw one?
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    %?debugFmt("~p: DOWN", [iolist_to_binary(pushy_event:get_time_as_iso8601(erlang:now()))]),
    State1 = remove_subscriber(Pid, State),
    {noreply, State1};
handle_info(_Info, State) ->
    ?debugVal({'pushy_org_events info UNEXPECTED', _Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions

handle_incoming_event(Ev = #event{name = <<"start">>}, JobId, State) ->
    add_raw_event(State, JobId, Ev);
handle_incoming_event(Ev = #event{name = <<"job_complete">>}, JobId, State) ->
    add_raw_event(State, JobId, Ev);
handle_incoming_event(_, _, State) ->
    % Ignore any other event
    State.

add_subscriber(Pid, State = #state{subscribers = Ss}) ->
    % Given how webmachine works, there is no way of knowing that an HTTP request has closed,
    % except that the process went down.  Not a bad backup to watch for the subscriber exit
    % anyway.
    monitor(process, Pid),
    State#state{subscribers = [Pid | Ss]}.

remove_subscriber(Pid, State = #state{subscribers = Ss}) ->
    State#state{subscribers = [P || P <- Ss, P /= Pid]}.

post_to_subscribers(#state{subscribers = Subscribers}, Msg) ->
    %case Msg of
        %#event{name = Name} -> ?debugVal({Subscribers, Name});
        %_ -> ?debugVal({Subscribers, Msg})
    %end,
    lists:foreach(fun(W) -> W ! Msg end, Subscribers).

% XXX Event-creation code copied from pushy_job_state -- refactor?
%add_event(State = #state{next_event_id = Id}, EventName, PropList) ->
    %Ev = pushy_event:make_event(EventName, Id, PropList),
    %add_raw_event(State, Ev).

add_raw_event(State = #state{next_event_id = Id}, JobId, RawEvent = #event{data = Data}) ->
    State1 = expire_old_events(State),
    RevEvents = State1#state.rev_events,
    IdStr = iolist_to_binary(io_lib:format("org-~B", [Id])), 
    Ev = RawEvent#event{
           id = IdStr,  % update the id for our event stream
           data = [{<<"job">>, JobId} | Data]
          },
    post_to_subscribers(State1, {ev, Ev}),
    State1#state{next_event_id = Id + 1, rev_events = [Ev | RevEvents]}.

% A version of lists:takewhile, that returns 'no_match' if the predicate doesn't match any entries.
take(F, L) -> take(F, L, []).

take(_F, [], _A) -> no_match;
take(F, [H|T], A) -> case F(H) of
                         true -> take(F, T, [H|A]);
                         false -> A
                     end.

start_of_history_event() ->
    % XX This calculation only works correctly if the expiration is less than 100000 seconds (~ 12 days)
    Expiration = get_expiration(),
    {MS, S, US} = os:timestamp(),
    ExpirationTime = case (S - Expiration) < 0 of
                         true -> {MS-1, S + 1000000 - Expiration, US};
                         false -> {MS, S - Expiration, US}
                     end,
    #event{name = <<"start_of_history">>, id = <<"start_id">>, timestamp = ExpirationTime, data = []}.

build_event_response(LastEventID, State) ->
    RevEvents = State#state.rev_events,
    case LastEventID of
            undefined -> [];
            % The events are in reverse order, so we want the beginning of the list, not the end.
            % Happily, this does the right thing if the LastEventId doesn't exist.
            S -> 
                case take(fun(E) -> E#event.id /= list_to_binary(S) end, RevEvents) of
                    no_match -> [start_of_history_event() | lists:reverse(RevEvents)];
                    TakenEvents -> lists:reverse(TakenEvents)
                end
    end.

expire_old_events(State) -> 
    Now = os:timestamp(),
    ExpirationTime = get_expiration() * 1000000,
    NewEvs = [E || E <- State#state.rev_events, timer:now_diff(Now, E#event.timestamp) =< ExpirationTime],
    State#state{rev_events = NewEvs}.

get_expiration() ->
    envy:get(pushy, org_feed_expiration, 60, integer).
