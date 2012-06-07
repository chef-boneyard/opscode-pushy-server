%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(pushy_job_runner).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

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
            {next_state, executing, register_node_status_watchers(save_job_status(executing, Job))};
        false ->
            error_logger:error_msg("Failed to register job tracker process ~p for ~p~n", [JobId, self()]),
            {stop, shutdown, Job}
    end.

handle_event(_Event, StateName, Job) ->
    {next_state, StateName, Job}.

handle_sync_event(_Event, _From, StateName, Job) ->
    {reply, ok, StateName, Job}.

handle_info(error, executing, Job) ->
    error_logger:info_msg("JOB STATUS ERROR~n"),
    {next_state, error, save_job_status(error, Job)};
handle_info(failed, executing, Job) ->
    error_logger:info_msg("JOB STATUS FAILED~n"),
    {next_state, failed, save_job_status(failed, Job)};
handle_info(expired, new, Job) ->
    error_logger:info_msg("JOB STATUS EXPIRED~n"),
    {next_state, expired, save_job_status(expired, Job)};
handle_info(expired, executing, Job) ->
    error_logger:info_msg("JOB STATUS EXPIRED~n"),
    {next_state, expired, save_job_status(expired, Job)};
handle_info(aborted, executing, Job) ->
    error_logger:info_msg("JOB STATUS ABORTED~n"),
    {next_state, aborted, save_job_status(aborted, Job)};
handle_info({node_state_change, NodeName, Status}, StateName, Job) ->
    error_logger:info_msg("JOB STATUS NODE STATE CHANGE~n"),
    {next_state, StateName, job_complete_check(save_job_node_status(Status, NodeName, Job))};
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
register_node_status_watchers([], Job) ->
    Job;
register_node_status_watchers([#pushy_job_node{node_name = NodeName}|Rest], Job) ->
    pushy_node_state:start_watching(NodeName),
    register_node_status_watchers(Rest, Job).
register_node_status_watchers(#pushy_job{job_nodes=JobNodes}=Job) ->
    register_node_status_watchers(JobNodes, Job).


status_complete(#pushy_job_node{status=Status}) ->
    lists:member(Status, ?COMPLETE_STATUS).

job_complete_check(#pushy_job{job_nodes=JobNodes}=Job) ->
    case lists:all(fun status_complete/1, JobNodes) of
        true ->
            gen_fsm:send_event(self(), {next_state, complete, Job})
    end,
    Job.

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

