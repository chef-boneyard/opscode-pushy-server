%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_job_state).

-behaviour(gen_fsm).

%% API
-export([start_link/4]).

%% States
-export([initializing/2]).

%% gen_fsm callbacks
-export([init/1,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include_lib("eunit/include/eunit.hrl").

-type job_id() :: binary().
-type node_name() :: binary().
-type org_id() :: binary().
-type possible_states() :: 'initializing' | 'voting'.


-record(job_state,
        {
            job_id     :: job_id(),
            org_id     :: org_id(),
            command    :: binary(),
            node_names :: [node_name()]
        }).

%%%
%%% External API
%%%
-spec start_link(job_id(), org_id(), binary(), [node_name()]) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link(JobId, OrgId, Command, NodeNames) ->
    gen_fsm:start_link(?MODULE, {JobId, OrgId, Command, NodeNames}, []).

%
% gen_fsm
%

%
% This is split into two phases: an 'upper half' to get the minimimal work done required to wire things up
% and a 'lower half' that takes care of things that can wait
%
-spec init({job_id(), [node_name()]}) ->
    {'ok', 'initializing', #job_state{}, 0} |
    {'stop', 'shutdown', #job_state{}}.
init({JobId, OrgId, Command, NodeNames}) ->
    lager:info("pushy_job_state:init(~p, ~p, ~p, ~p)", [JobId, OrgId, Command, NodeNames]),
    State = #job_state{
                job_id = JobId,
                org_id = OrgId,
                command = Command,
                node_names = NodeNames
            },
    try
        %% The most important thing to have happen is this registration; we need to get this
        %% assigned before anyone else tries to start things up gproc:reg can only return
        %% true or throw
        true = gproc:reg({n, l, JobId}),
        {ok, initializing, State, 0}
    catch
        error:badarg ->
            %% When we start up from a previous run, we have two ways that the FSM might be started;
            %% from an incoming packet, or the database record for a prior run
            %% There may be some nasty race conditions surrounding this.
            %% We may also want to *not* automatically reanimate FSMs for nodes that aren't
            %% actively reporting; but rather keep them in a 'limbo' waiting for the first
            %% packet, and if one doesn't arrive within a certain time mark them down.
            lager:error("Failed to register:~p for ~p (already exists as ~p?)",
                        [JobId,self(), gproc:lookup_pid({n,l,JobId}) ]),
            {stop, shutdown, State}
    end.

-spec handle_event(any(), possible_states(), #job_state{}) ->
        {'next_state', possible_states(), #job_state{}}.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(any(), any(), possible_states(), #job_state{}) ->
        {'reply', 'ok', possible_states(), #job_state{}}.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

-spec handle_info(any(), possible_states(), #job_state{}) ->
        {'next_state', possible_states(), #job_state{}}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(any(), possible_states(), #job_state{}) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(any(), possible_states(), #job_state{}, any()) ->
        {'ok', possible_states(), #job_state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%
% Lower half of initialization; we have more time for complex work here.
%

-spec initializing('timeout', #job_state{}) ->
                    {'next_state', 'voting', #job_state{}}.
initializing(timeout, State) ->
    lager:info("pushy_job_state:initializing(timeout, ~p)", [State]),
    % Upon transitioning to voting, send the command message to all nodes.
    send_command_message_to_all_nodes(State),
    {next_state, voting, State}.

%
% PRIVATE
%
-spec send_command_message_to_all_nodes(#job_state{}) -> 'ok'.
send_command_message_to_all_nodes(#job_state{job_id = JobId,
                                             org_id = OrgId,
                                             command = Command,
                                             node_names = NodeNames}) ->
    CommandMessage = jiffy:encode({[
        {type, <<"job_command">>},
        {job_id, JobId},
        {command, Command},
        {server, get_server_name()}
    ]}),
    pushy_command_switch:send_multi_command(OrgId, NodeNames, CommandMessage).

-spec get_server_name() -> binary().
get_server_name() ->
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    list_to_binary(Host).

