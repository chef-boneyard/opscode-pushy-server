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
-define(NO_JOB, {error, no_job}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([load_from_db/2,
         register_process/2]).

-include_lib("pushy_sql.hrl").

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         node_state_change/3,
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
    {ok, load_from_db, #pushy_job{id=JobId}, 0}.

load_from_db(timeout, #pushy_job{id=JobId}=Job) ->
    case pushy_sql:fetch_job(JobId) of
        {ok, #pushy_job{}=Job1} ->
            {next_state, register_process, Job1, 0};
        {ok, not_found} ->
            error_logger:error_msg("Failed to find job ~p for ~p~n", [JobId, self()]),
            {stop, not_found, Job}
    end.

register_process(timeout, #pushy_job{id=JobId, duration=Duration}=Job) ->
    case gproc:reg({n, l, JobId}) of
        true ->
            %% TODO compute timeout based on existing created_at
            erlang:send_after(Duration*1000, self(), expired),
            error_logger:info_msg("Beginning execution of job ~s~n", [JobId]),
            {next_state, executing, execute_job(Job)};
        false ->
            error_logger:error_msg("Failed to register job tracker process ~p for ~p~n", [JobId, self()]),
            {stop, shutdown, Job}
    end.

node_state_change(JobId, NodeName, Type) ->
    case catch gproc:send({n,l,JobId},
            {node_state_change, NodeName, Type}) of
        {'EXIT', _} -> ?NO_JOB;
        _ -> ok
    end.

handle_event(_Event, StateName, Job) ->
    {next_state, StateName, Job}.

handle_sync_event(_Event, _From, StateName, Job) ->
    {reply, ok, StateName, Job}.

handle_info(error, executing, Job) ->
    error_logger:info_msg("JOB ERROR~n"),
    {next_state, error, save_job_status(error, Job)};
handle_info(failed, executing, Job) ->
    error_logger:info_msg("JOB FAILED~n"),
    {next_state, failed, save_job_status(failed, Job)};
handle_info(expired, new, Job) ->
    error_logger:info_msg("JOB EXPIRED~n"),
    {next_state, expired, save_job_status(expired, Job)};
handle_info(expired, executing, Job) ->
    error_logger:info_msg("JOB EXPIRED~n"),
    {next_state, expired, save_job_status(expired, Job)};
handle_info(aborted, executing, Job) ->
    error_logger:info_msg("JOB ABORTED~n"),
    {next_state, aborted, save_job_status(aborted, Job)};
handle_info(complete, executing, Job) ->
    error_logger:info_msg("JOB COMPLETE~n"),
    %% TODO - clean up job
    {next_state, complete, save_job_status(complete, Job)};
handle_info({node_state_change, NodeName, NodeState}, StateName, Job) ->
    TranslatedNodeState = hb_to_job_status(NodeState),
    error_logger:info_msg("JOB NODE ~p STATE CHANGE ~p~n", [NodeName, TranslatedNodeState]),
    job_complete_check(save_job_node_status(TranslatedNodeState, NodeName, Job), StateName);
handle_info(Info, StateName, Job) ->
    error_logger:info_msg("JOB STATUS CATCH ALL: Info->~p StateName->~p~n", [Info,StateName]),
    {next_state, StateName, Job}.

terminate(_Reason, _StateName, _Job) ->
    ok.

code_change(_OldVsn, StateName, Job, _Extra) ->
    {ok, StateName, Job}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% TODO - do we really need these small diferences between heartbeat types and
%%        actual underlying job status.
hb_to_job_status(running) ->
    executing;
hb_to_job_status(idle) ->
    complete;
hb_to_job_status(CatchAll) ->
    CatchAll.

execute_job(#pushy_job{job_nodes=JobNodes}=Job) ->
    NodeNames = [ JobNode#pushy_job_node.node_name || JobNode <- JobNodes ],
    register_node_status_watchers(NodeNames),
    pushy_command_switch:send_multi_command(?POC_ORG_NAME, NodeNames, create_command_message(Job)),
    save_job_status(executing, Job).

create_command_message(#pushy_job{id=JobId, command=Command}) ->
    Host = pushy_util:get_env(pushy, server_name, fun is_list/1),
    jiffy:encode({[{server, list_to_binary(Host)},
                    {type, <<"job_command">>},
                    {job_id, JobId},
                    {command, Command}]}).

register_node_status_watchers([]) ->
    ok;
register_node_status_watchers([NodeName|Rest]) ->
    pushy_node_state:start_watching(NodeName),
    register_node_status_watchers(Rest).

status_complete(#pushy_job_node{status=Status}) ->
    lists:member(Status, ?COMPLETE_STATUS).

job_complete_check(#pushy_job{job_nodes=JobNodes}=Job, CurrentState) ->
    case lists:all(fun status_complete/1, JobNodes) of
        true ->
            {next_state, complete, save_job_status(complete, Job)};
        false ->
            {next_state, CurrentState, Job}
    end.

save_job_status(Status, Job) ->
    Job1 = Job#pushy_job{status=Status},
    case pushy_object:update_object(update_job, Job1, ?POC_ACTOR_ID) of
        {ok, _} ->
            Job1;
        {error, _Error} ->
            Job1
    end.

% Option 1
% save_job_node_status(Status, NodeName, #pushy_job{job_nodes=Nodes}=Job) ->
%     Fun1 = fun(#pushy_job_node{node_name=Name}=Node) ->
%                 if
%                     Name =:= NodeName ->
%                         Node1 = Node#pushy_job_node{status=Status},
%                         pushy_object:update_object(update_job_node, Node1),
%                         Node1;
%                     true ->
%                         Node
%                 end
%             end,
%     Nodes1 = lists:map(Fun1, Nodes),
%     Job#pushy_job{job_nodes=Nodes1}.

% Option 2
save_job_node_status(Status, NodeName, #pushy_job{job_nodes=Nodes}=Job) ->
    Fun1 = fun(#pushy_job_node{node_name=Name}) -> Name =:= NodeName end,
    {[Node | _], Rest} = lists:partition(Fun1, Nodes),
    Node1 = Node#pushy_job_node{status=Status},
    pushy_object:update_object(update_job_node, Node1),
    Job#pushy_job{job_nodes=[Node1 | Rest]}.

