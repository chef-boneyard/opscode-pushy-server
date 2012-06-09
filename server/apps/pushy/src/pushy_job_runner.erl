%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode, Inc.
%%% @doc
%%% FSM encapsulating life cycle of a single job.
%%% @end

-module(pushy_job_runner).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([load_from_db/2,
         register_process/2]).

-define(NO_JOB, {error, no_job}).
-define(JOB_NODE_EVENT(Event), Event(JobId, NodeName, Type) -> case catch gproc:send({n,l,JobId}, {Event, NodeName, Type}) of
                                              {'EXIT', _} -> ?NO_JOB;
                                              _ -> ok
                                          end).

-record(state, {job_id,
                job,
                acked_nodes=[],
                nacked_nodes=[]}).

-include_lib("pushy_sql.hrl").

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         node_command_event/3,
         node_heartbeat_event/3,
         terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(JobId) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [JobId], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([JobId]) ->
    {ok, load_from_db, #state{job_id=JobId}, 0}.

load_from_db(timeout, #state{job_id=JobId}=State) ->
    case pushy_sql:fetch_job(JobId) of
        {ok, #pushy_job{}=Job} ->
            {next_state, register_process, State#state{job=Job}, 0};
        {ok, not_found} ->
            error_logger:error_msg("Failed to find job ~p for ~p~n", [JobId, self()]),
            {stop, not_found, State}
    end.

register_process(timeout, #state{job_id=JobId, job=#pushy_job{duration=Duration}}=State) ->
    case gproc:reg({n, l, JobId}) of
        true ->
            %% TODO compute timeout based on existing created_at
            erlang:send_after(Duration*1000, self(), expired),
            error_logger:info_msg("Beginning execution of job ~s~n", [JobId]),
            {next_state, executing, execute_job(State)};
        false ->
            error_logger:error_msg("Failed to register job tracker process ~p for ~p~n", [JobId, self()]),
            {stop, shutdown, State}
    end.

?JOB_NODE_EVENT(node_heartbeat_event).
?JOB_NODE_EVENT(node_command_event).

handle_event(complete, _StateName, #state{job_id=JobId}=State) ->
    error_logger:info_msg("Job [~p] complete.~n", [JobId]),
    {stop, job_complete, save_job_status(complete, State)};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_info(expired, _StateName, #state{job_id=JobId}=State) ->
    error_logger:info_msg("Job [~p] expired.~n", [JobId]),
    {stop, job_expired, save_job_status(expired, State)};
handle_info({node_command_event, NodeName, ack}, StateName,
                #state{job_id=JobId, acked_nodes=AckedNodes}=State) ->
    error_logger:info_msg("Job [~p] ACK received for node [~p].~n", [JobId, NodeName]),
    {next_state, StateName,
        execute_start_check(State#state{acked_nodes = [NodeName|AckedNodes]})};
handle_info({node_command_event, NodeName, nack}, StateName,
                #state{job_id=JobId, nacked_nodes=NackedNodes}=State) ->
    error_logger:info_msg("Job [~p] NACK received for node [~p].~n", [JobId, NodeName]),
    {next_state, StateName,
        execute_start_check(State#state{nacked_nodes = [NodeName|NackedNodes]})};
handle_info({node_command_event, NodeName, CommandEvent}, StateName, State) ->
    Status = event_to_job_status(CommandEvent),
    error_logger:info_msg("Node [~p] job status changed [~p]~n", [NodeName, Status]),
    {next_state, StateName, job_complete_check(save_job_node_status(Status, NodeName, State))};
handle_info({node_heartbeat_event, NodeName, down}, StateName, State) ->
    error_logger:info_msg("Node [~p] job status changed [failed]~n", [NodeName]),
    {next_state, StateName, job_complete_check(save_job_node_status(failed, NodeName, State))};
handle_info(Info, StateName, State) ->
    error_logger:info_msg("JOB STATUS CATCH ALL: Info->~p StateName->~p~n", [Info,StateName]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% TODO - do we really need these small diferences between heartbeat types and
%%        actual underlying job status.
event_to_job_status(started) ->
    executing;
event_to_job_status(finished) ->
    complete.

execute_job(#state{job=#pushy_job{job_nodes=JobNodes}=Job}=State) ->
    NodeNames = [ JobNode#pushy_job_node.node_name || JobNode <- JobNodes ],
    %[ pushy_node_state:start_watching(NodeName) || NodeName <- NodeNames],
    pushy_command_switch:send_multi_command(?POC_ORG_NAME, NodeNames, create_message({command, Job})),
    save_job_status(executing, State).

create_message(Data) when is_list(Data) ->
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    jiffy:encode({[{server, list_to_binary(Host)} | Data]});
create_message({command, #pushy_job{id=JobId, command=Command}}) ->
    create_message([{type, <<"job_command">>},
                    {job_id, JobId},
                    {command, Command}]);
create_message({execute, #pushy_job{id=JobId}}) ->
    create_message([{type, <<"job_execute">>},
                    {job_id, JobId}]).

status_complete(#pushy_job_node{status=Status}) ->
    lists:member(Status, ?COMPLETE_STATUS).

job_complete_check(#state{job=#pushy_job{job_nodes=JobNodes}}=State) ->
    case lists:all(fun status_complete/1, JobNodes) of
        true ->
            gen_fsm:send_all_state_event(self(), complete),
            State;
        false ->
            State
    end.

execute_start_check(#state{acked_nodes=AckedNodes, nacked_nodes=NackedNodes,
                        job=#pushy_job{job_nodes=JobNodes}=Job}=State) ->
    case lists:flatlength(JobNodes) == lists:flatlength([AckedNodes|NackedNodes]) of
        true ->
            pushy_command_switch:send_multi_command(?POC_ORG_NAME, AckedNodes, create_message({execute, Job})),
            State;
        false ->
            State
    end.

save_job_status(Status, #state{job=Job}=State) ->
    Job1 = Job#pushy_job{status=Status},
    case pushy_object:update_object(update_job, Job1, ?POC_ACTOR_ID) of
        {ok, _} ->
            State#state{job=Job1};
        {error, _Error} ->
            State#state{job=Job1}
    end.

save_job_node_status(Status, NodeName, #state{job=#pushy_job{job_nodes=Nodes}=Job}=State) ->
    Fun1 = fun(#pushy_job_node{node_name=Name}) -> Name =:= NodeName end,
    {[Node | _], Rest} = lists:partition(Fun1, Nodes),
    Node1 = Node#pushy_job_node{status=Status},
    pushy_object:update_object(update_job_node, Node1),
    State#state{job=Job#pushy_job{job_nodes=[Node1 | Rest]}}.

