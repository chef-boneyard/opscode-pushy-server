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

-export([start_link/1,
         node_command_event/3,
         node_heartbeat_event/3]).

%% ------------------------------------------------------------------
%% State Function Exports
%% ------------------------------------------------------------------

-export([load_from_db/2,
         register_process/2,
         start_vote/2]).

%% ------------------------------------------------------------------
%% Event Handler Function Exports
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

% time in milliseconds to wait for all nodes to ACK/NACK if they can execute the
% job's command.  If quorum is met we run it, otherwise the job is marked failed.
-define(JOB_QUORUM_THRESHOLD_TIMEOUT, 10000). % 10 seconds
-define(NO_JOB, {error, no_job}).
-define(JOB_NODE_EVENT(Event), Event(JobId, NodeName, Type) -> case catch gproc:send({n,l,JobId}, {Event, NodeName, Type}) of
                                              {'EXIT', _} -> ?NO_JOB;
                                              _ -> ok
                                          end).

-record(state, {job_id,
                job,
                acked_nodes=[],
                nacked_nodes=[],
                qtref,
                quorum}).

-include_lib("pushy_sql.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(JobId) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [JobId], []).

?JOB_NODE_EVENT(node_heartbeat_event).
?JOB_NODE_EVENT(node_command_event).

%% ------------------------------------------------------------------
%% State Function Definitions
%% ------------------------------------------------------------------

load_from_db(timeout, #state{job_id=JobId}=State) ->
    case pushy_sql:fetch_job(JobId) of
        {ok, #pushy_job{job_nodes=JobNodes}=Job} ->
            %% TODO - eventually quorum will be configurable, for now we'll keep
            %% it simple and say all nodes must ACK.
            Quorum = lists:flatlength(JobNodes),
            {next_state, register_process, State#state{quorum=Quorum, job=Job}, 0};
        {ok, not_found} ->
            error_logger:error_msg("Failed to find job ~p for ~p~n", [JobId, self()]),
            {stop, {error, not_found}, State}
    end.

register_process(timeout, #state{job_id=JobId}=State) ->
    case gproc:reg({n, l, JobId}) of
        true ->
            {next_state, start_vote, State, 0};
        false ->
            error_logger:error_msg("Failed to register job tracker process ~p for ~p~n", [JobId, self()]),
            {stop, {error, not_registered}, State}
    end.

start_vote(timeout, #state{job_id=JobId}=State) ->
    error_logger:info_msg("Commencing vote for job ~s~n", [JobId]),
    {next_state, tally_vote, start_voting(State)}.

%% ------------------------------------------------------------------
%% Event Handler Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([JobId]) ->
    {ok, load_from_db, #state{job_id=JobId}, 0}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%% Timeout message handlers..triggered by a erlang:send_after/3
%% ANYTHING -> expired
handle_info({expired, Reason}, _StateName, #state{job_id=JobId}=State) ->
    error_logger:info_msg("Job [~p] expired for reason: ~p.~n", [JobId,Reason]),
    %% TODO - instruct all nodes to ABORT
    {stop, normal, save_job_status(expired, State)};
%% ANYTHING -> failed
handle_info({failed, Reason}, _StateName, #state{job_id=JobId}=State) ->
    error_logger:info_msg("Job [~p] failed for reason: ~p.~n", [JobId,Reason]),
    %% TODO - instruct all nodes to ABORT
    {stop, normal, save_job_status(failed, State)};

%% out of band messages (mostly other process updating us of our nodes' progress)
handle_info({node_command_event, NodeName, ack}, StateName,
                #state{job_id=JobId, acked_nodes=AckedNodes}=State) ->
    error_logger:info_msg("Job [~p] ACK received for node [~p].~n", [JobId, NodeName]),
    execute_start_check(StateName, State#state{acked_nodes = [NodeName|AckedNodes]});
handle_info({node_command_event, NodeName, nack}, StateName,
                #state{job_id=JobId, nacked_nodes=NackedNodes}=State) ->
    error_logger:info_msg("Job [~p] NACK received for node [~p].~n", [JobId, NodeName]),
    execute_start_check(StateName, State#state{nacked_nodes = [NodeName|NackedNodes]});
handle_info({node_command_event, NodeName, CommandEvent}, StateName, State) ->
    Status = event_to_job_status(CommandEvent),
    error_logger:info_msg("Node [~p] job status changed [~p]~n", [NodeName, Status]),
    job_complete_check(StateName, save_job_node_status(Status, NodeName, State));
handle_info({node_heartbeat_event, NodeName, down}, StateName, State) ->
    error_logger:info_msg("Node [~p] job status changed [failed]~n", [NodeName]),
    job_complete_check(StateName, save_job_node_status(failed, NodeName, State));
handle_info(_Info, StateName, State) ->
    % error_logger:info_msg("Job runner catch-all: Message -> ~p Current State -> ~p~n", [Info,StateName]),
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

%% creates a well-formed JSON message for the pushy_command_switch to send
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

%% Sends the initial job_command message out to nodes who reply with an ACK/NACK
%% depending on if they are available to execute the Job's command.  Two timeouts
%% are created, one for over all job execution expiration and one which gives nodes
%% time to ACK/NACK before final quorum check is performed.
start_voting(#state{job=#pushy_job{duration=Duration, job_nodes=JobNodes}=Job}=State) ->
    NodeNames = [ JobNode#pushy_job_node.node_name || JobNode <- JobNodes ],
    %% total job expiration timeout
    %% TODO compute timeout based on existing created_at
    erlang:send_after(Duration*1000, self(), {expired, execution_duration_up}),
    %% quorum timeout - we can only wait a set time for ACK/NACKs from this Job's nodes
    QTRef = erlang:send_after(?JOB_QUORUM_THRESHOLD_TIMEOUT, self(), {failed, no_quorum}),
    %% subscribe to real-time node status updates via the heartbeat channel
    [ pushy_node_state:start_watching(NodeName) || NodeName <- NodeNames],
    %% Send the voting message out; we expect an ACK/NACK back from our nodes.
    %% The quorum timeout should cover the case where some nodes don't vote (how rude).
    pushy_command_switch:send_multi_command(?POC_ORG_NAME, NodeNames, create_message({command, Job})),
    save_job_status(voting, State#state{qtref=QTRef}).

%% Sends the job_execute message to all nodes who have ACKED
start_executing(#state{acked_nodes=AckedNodes, job=Job}=State) ->
    %% send a job_execute message to all acked nodes
    pushy_command_switch:send_multi_command(?POC_ORG_NAME,
                                            AckedNodes,
                                            create_message({execute, Job})),
    save_job_status(executing, State).

%% First checks if all nodes have reported back.  If quorum has been met, the
%% quorum timeout timer is cancelled and all nodes who have ACKED are instructed
%% to begin executing the job's command.
execute_start_check(CurrentState,
                        #state{job_id=JobId,
                                acked_nodes=AckedNodes, nacked_nodes=NackedNodes,
                                quorum=Quorum, qtref=QTRef,
                                job=#pushy_job{job_nodes=JobNodes}}=State) ->
    %% first we check if everyone has voted
    case lists:flatlength(JobNodes) == lists:flatlength([AckedNodes|NackedNodes]) of
        true ->
            %% if so we check if quorum has been met
            case Quorum == lists:flatlength(AckedNodes) of
                true ->
                    %% cancel the quorom wait timeout timer
                    erlang:cancel_timer(QTRef),
                    error_logger:info_msg("Beginning execution of Job [~p].~n", [JobId]),
                    {next_state, execute, start_executing(State)};
                false ->
                    error_logger:info_msg("Quorum could not be met, Job [~p] will not be run.~n", [JobId]),
                    {stop, normal, save_job_status(failed, State)}
                end;
        false ->
            {next_state, CurrentState, State}
    end.

%% Checks if all ACKED nodes have completed running the job.  This does not
%% necessarily mean the job was successful.
job_complete_check(CurrentState, #state{job_id=JobId,
                                        job=#pushy_job{job_nodes=JobNodes}}=State) ->
    %% Check if all nodes have returned a status that is considered complete
    case lists:all(
            fun(#pushy_job_node{status=Status}) ->
                lists:member(Status, ?COMPLETE_STATUS)
            end,
            JobNodes) of
        true ->
            error_logger:info_msg("Job [~p] complete.~n", [JobId]),
            %% TODO - mark job status 'error' if any nodes encountered issues
            {stop, normal, save_job_status(complete, State)};
        false ->
            {next_state, CurrentState, State}
    end.

%% Persists the overall job's status to the database.
save_job_status(Status, #state{job=Job}=State) ->
    Job1 = Job#pushy_job{status=Status},
    case pushy_object:update_object(update_job, Job1, ?POC_ACTOR_ID) of
        {ok, _} ->
            State#state{job=Job1};
        {error, _Error} ->
            State#state{job=Job1}
    end.

%% Persists the node's job status to the database.
save_job_node_status(Status, NodeName, #state{job=#pushy_job{job_nodes=Nodes}=Job}=State) ->
    Fun1 = fun(#pushy_job_node{node_name=Name}) -> Name =:= NodeName end,
    {[Node | _], Rest} = lists:partition(Fun1, Nodes),
    Node1 = Node#pushy_job_node{status=Status},
    pushy_object:update_object(update_job_node, Node1),
    State#state{job=Job#pushy_job{job_nodes=[Node1 | Rest]}}.

