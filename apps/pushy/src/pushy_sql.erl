%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2011-2012 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_sql).

-include_lib("pushy_sql.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, lager_transform}]).

-export([
         %% job ops
         fetch_job/1,
         fetch_jobs/1,
         fetch_incomplete_jobs/0,
         fetch_incomplete_job_nodes/0,
         create_job/1,
         update_job/1,
         update_job_node/1,
         insert_job_output/4,
         fetch_job_output/3,

         statements/0,

         sql_date/1
        ]).

%% job ops

-spec fetch_job(JobId :: object_id()) ->
    {ok, not_found | #pushy_job{}} | {error, term()}.
fetch_job(JobId) ->
    case sqerl:select(find_job_by_id, [JobId]) of
        {ok, none} ->
            {ok, not_found};
        {ok, Rows} ->
            {ok, job_join_rows_to_record(Rows)};
        {error, Error} ->
            lager:info("ERROR"),
            {error, Error}
    end.

-spec fetch_incomplete_jobs() -> {ok, [ #pushy_job{} ] } | {error, term()}.
fetch_incomplete_jobs() ->
    case sqerl:select(find_incomplete_jobs, []) of
        {ok, none} ->
            {ok, []};
        {ok, Rows} ->
            {ok, [prepare_incomplete_job(Row) || Row <- Rows]};
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_jobs(OrgId :: object_id() ) -> {ok, [ #pushy_job{} ] } | {error, term()}.
fetch_jobs(OrgId) ->
    case sqerl:select(find_jobs_by_org, [OrgId]) of
        {ok, none} ->
            {ok, []};
        {ok, Rows} ->
            % Note that we are not including node information here.  I'm not sure whether
            % that's an oversight, or because it's easier (since the node information results
            % in multiple rows too, meaning you can't simply invoke job_join_rows_to_record/1
            % -- SLG
            Jobs = lists:map(fun(R) ->
                        Job = prepare_pushy_job_record(R),
                        O = proplist_to_job_options(R),
                        Job#pushy_job{opts = O}
                             end, Rows),
            {ok, Jobs};
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_incomplete_job_nodes() -> {ok, [ #pushy_job_node{} ] } | {error, no_connections | {_,_}}.
fetch_incomplete_job_nodes() ->
    case sqerl:select(find_incomplete_job_nodes, []) of
        {ok, none} ->
            {ok, []};
        {ok, Rows} ->
            {ok, [prepare_incomplete_job_nodes(Row) || Row <- Rows]};
        {error, Error} ->
            {error, Error}
    end.

-spec create_job(#pushy_job{}) -> {ok, 1} | {error, term()}.
create_job(#pushy_job{job_nodes = JobNodes}=Job) ->
    Fields0 = flatten_record(Job),
    Fields = job_fields_for_insert(Fields0),
    %% We're not dispatching to the general create_object/2 because creating a job
    %% involves adding multiple rows to multiple tables. Also, we currently embed a list of
    %% job nodes in the job record; this won't play nicely with the general
    %% 'create_object' logic, which passes all record fields (in order) as parameters for a
    %% prepared statement
    case create_object(insert_job, Fields) of
        {ok, 1} ->
            case insert_job_nodes(JobNodes) of
                ok ->
                    case insert_job_options(Job) of
                        {ok, _} ->
                            %% (Remember, create_object/1 should return {ok, Number})
                            {ok, 1};
                        {error, Reason1} ->
                            %% We could have potentially inserted some job node rows before the
                            %% error was thrown. If we had transactions, we could just bail out here, but
                            %% instead we need to do a little cleanup. Fortunately, this just means
                            %% deleting all rows with the given job id.

                            % delete_job_nodes(JobId),

                            %% Finally, we'll pass the root cause of
                            %% the failure back up
                            parse_error(Reason1)
                    end;
                {error, Reason} ->
                    %% We could have potentially inserted some job node rows before the
                    %% error was thrown. If we had transactions, we could just bail out here, but
                    %% instead we need to do a little cleanup. Fortunately, this just means
                    %% deleting all rows with the given job id.

                    % delete_job_nodes(JobId),

                    %% Finally, we'll pass the root cause of
                    %% the failure back up
                    parse_error(Reason)
            end;
        {error, Reason} ->
            parse_error(Reason)
    end.

-spec update_job(#pushy_job{}) -> {ok, 1 | not_found} | {error, no_connections | {_,_}}.
update_job(#pushy_job{id = JobId,
                      status = Status,
                      last_updated_by = LastUpdatedBy,
                      updated_at = UpdatedAt}) ->
    UpdateFields = [Status, LastUpdatedBy, UpdatedAt, JobId],
    do_update(update_job_by_id, UpdateFields).

-spec update_job_node(#pushy_job_node{}) -> {ok, 1 | not_found} | {error, no_connections | {_,_}}.
update_job_node(#pushy_job_node{job_id = JobId,
                                node_name = NodeName,
                                org_id = OrgId,
                                updated_at = UpdatedAt,
                                status = Status}) ->
    UpdateFields = [job_node_status(Status), UpdatedAt, OrgId, NodeName, JobId],
    do_update(update_job_node_by_orgid_nodename_jobid, UpdateFields).

-spec create_object(atom(), list()) -> {ok, non_neg_integer()} | {error, term()}.
create_object(QueryName, Args) ->
    case sqerl:statement(QueryName, Args, count) of
        {ok, N} ->
            {ok, N};
        {conflict, Reason} ->
            {conflict, Reason};
        {error, Reason} ->
            parse_error(Reason)
        %% FIXME: original code for create_node had the following match, but seems like
        %% crashing would be better if we get an unexpected error.
        %% Error -> Error
    end.

%% @doc removes any lists from the list of #pushy_job{} values; in other words, remove the
%% 'job_nodes' field.  We don't use that to insert new rows into the jobs table (that data's
%% joined).
%% Similarly, remove any tuples (i.e. records); in other words, remove the optional fields
%% record, "opts".  Those fields are potentially undefined, and will go into a separate table,
%% "job_options", in the database.
%% @end
%%
%% TODO: There is a better, less opaque way to achieve this.
job_fields_for_insert(JobFields) ->
    Pred = fun(Elem) ->
                   not(is_list(Elem)) andalso not(is_tuple(Elem))
           end,
    lists:filter(Pred, JobFields).

-spec insert_job_nodes([#pushy_job_node{}]) -> ok | {error, no_connections | {_,_}}.
%% @doc Inserts job_nodes records into the database. All records are timestamped
%% with the same stamp, namely `CreatedAt`, which is a binary string in SQL date time
%% format.
%%
%% Returns 'ok' if all records are inserted without issue. Returns an error tuple on the
%% first checksum that fails to insert into the database for whatever reason. Further
%% processing of the list is abandoned at that point.
insert_job_nodes([]) ->
    ok;
insert_job_nodes([#pushy_job_node{job_id=JobId,
                                    org_id=OrgId,
                                    node_name=NodeName,
                                    status=Status,
                                    created_at=CreatedAt,
                                    updated_at=UpdatedAt}|Rest]) ->
    case sqerl:statement(insert_job_node, [JobId, OrgId, NodeName,
            job_node_status(Status), CreatedAt, UpdatedAt], count) of
        {ok, 1} ->
            insert_job_nodes(Rest);
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert_job_options(#pushy_job{}) -> ok | {error, no_connections | {_,_}}.
%% @doc Inserts job_options records into the database.
insert_job_options(#pushy_job{opts = #pushy_job_opts{user = undefined,
                              dir = undefined,
                              env = undefined,
                              capture = false,
                              file = undefined}}) ->
    {ok, 0};
insert_job_options(#pushy_job{id = Id, opts = #pushy_job_opts{
                              user = User,
                              dir = Dir,
                              env = Env,
                              capture = Capture,
                              file = File}}) ->
    %?debugVal({Id, User, Dir, Env, Capture, File}),
    EncUser = undef_to_null(User),
    EncDir = undef_to_null(Dir),
    EncEnv = case Env of
                 undefined -> null;
                 _ -> jiffy:encode(Env)
             end,
    EncFile = undef_to_null(File),
    sqerl:statement(insert_job_options,
                    [Id, EncUser, EncDir, EncEnv, Capture, EncFile]).

undef_to_null(undefined) -> null;
undef_to_null(O) -> O.

%% sequel returns seconds as float, while many erlang functions want integer
%% we trunc instead of rounding because if it's 59.6 we don't want to round to 60!
%% TODO: Move this fn to some common code
%%
trunc_date_time_to_second({{YY,MM,DD},{H,M,S}}) ->
    {{YY,MM,DD},{H,M, erlang:trunc(S)}}.

-spec job_join_rows_to_record(Rows :: [proplists:proplist()]) ->  #pushy_job{}.
%% @doc Transforms a collection of proplists representing a job / job_nodes join query
%% result and collapses them all into a single job record. There is a row for each
%% job_node. A job_node tuple is extracted from each row; job information is extracted
%% from the final row (since it's the same in every row).
%%
%% See the 'find_job_by_id' prepared query for the row "shape".
job_join_rows_to_record(Rows) ->
    job_join_rows_to_record(Rows, []).

-spec job_join_rows_to_record(Rows :: [proplists:proplist()], [#pushy_job_node{}]) -> #pushy_job{}.
job_join_rows_to_record([LastRow|[]], JobNodes) ->
    C = proplist_to_job_node(LastRow),
    O = proplist_to_job_options(LastRow),
    Job = prepare_pushy_job_record(LastRow),
    Job#pushy_job{job_nodes = lists:flatten(lists:reverse([C|JobNodes])),
                  opts = O};
job_join_rows_to_record([Row|Rest], JobNodes ) ->
    C = proplist_to_job_node(Row),
    job_join_rows_to_record(Rest, [C|JobNodes]).

date_time_to_sql_date(Date) ->
    Date0 = trunc_date_time_to_second(Date),
    iolist_to_binary(httpd_util:rfc1123_date(Date0)).

%%% Emit in DATETIME friendly format
%% @doc Convert an Erlang timestamp (see `os:timestamp/0') to DATETIME friendly format.
-spec sql_date(now | {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> binary().
sql_date(now) ->
    sql_date(os:timestamp());
sql_date({_,_,_} = TS) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second])).

prepare_pushy_job_record(Job) ->
    CreatedAt = safe_get(<<"created_at">>, Job),
    CreatedAtFormatted = date_time_to_sql_date(CreatedAt),
    UpdatedAt = safe_get(<<"updated_at">>, Job),
    UpdatedAtFormatted = date_time_to_sql_date(UpdatedAt),

    #pushy_job{id = safe_get(<<"id">>, Job),
               org_id = safe_get(<<"org_id">>, Job),
               command = safe_get(<<"command">>, Job),
               status = safe_get(<<"status">>, Job),
               run_timeout = safe_get(<<"run_timeout">>, Job),
               updated_at = UpdatedAtFormatted,
               created_at = CreatedAtFormatted,
               last_updated_by = safe_get(<<"last_updated_by">>, Job)}.

prepare_incomplete_job(Job) ->
    #pushy_job{id = safe_get(<<"id">>, Job),
               status = safe_get(<<"status">>, Job)}.

prepare_incomplete_job_nodes(Node) ->
    #pushy_job_node{job_id = safe_get(<<"job_id">>, Node),
                    org_id = safe_get(<<"org_id">>, Node),
                    node_name = safe_get(<<"node_name">>, Node)}.

-spec proplist_to_job_node(proplists:proplist()) -> #pushy_job_node{}.
%% @doc Convenience function for assembling a job_node tuple from a proplist
proplist_to_job_node(Proplist) ->
    case safe_get(<<"node_name">>, Proplist) of
        null -> [];
        _ ->
            #pushy_job_node{job_id = safe_get(<<"id">>, Proplist),
                org_id = safe_get(<<"org_id">>, Proplist),
                node_name = safe_get(<<"node_name">>, Proplist),
                status = job_node_status(safe_get(<<"job_node_status">>, Proplist)),
                created_at = date_time_to_sql_date(safe_get(<<"created_at">>, Proplist)),
                updated_at = date_time_to_sql_date(safe_get(<<"updated_at">>, Proplist))}
    end.

-spec proplist_to_job_options(proplists:proplist()) -> #pushy_job_opts{}.
%% @doc Convenience function for assembling a job_options tuple from a proplist
proplist_to_job_options(Proplist) ->
    DecEnv = case safe_get(<<"env">>, Proplist) of
                 null -> undefined;
                 O -> jiffy:decode(O)
             end,
    #pushy_job_opts{user = null_get(<<"job_user">>, Proplist),
                    dir = null_get(<<"dir">>, Proplist),
                    env = DecEnv,
                    % Even though capture is a boolean, it still could be null because we are
                    % doing an outer join with a row that may not exist
                    capture = null_get(<<"capture">>, Proplist, false),
                    file = null_get(<<"job_file">>, Proplist)}.

-spec insert_job_output(object_id(), binary(),
                        undefined | binary(), undefined | binary()) -> ok.
%% @doc Inserts captured stdout/stderr records into the database.
insert_job_output(_JobId, _NodeName, undefined, undefined) ->
    ok;
insert_job_output(JobId, NodeName, Stdout, Stderr) ->
    EncOut = undef_to_null(Stdout),
    EncErr = undef_to_null(Stderr),
    case sqerl:statement(insert_job_output, [JobId, NodeName, EncOut, EncErr]) of
        {error, Error} -> lager:error("Could not insert job output ~p: ~p",
                                      [{JobId, NodeName, EncOut, EncErr}, Error]);
        {ok, _} -> ok
    end,
    ok.

-spec fetch_job_output(binary(), binary(), stdout | stderr) -> {ok, binary()} | not_found.
%% @doc Retrieves the output of the specified job/node/channel, if any
fetch_job_output(JobId, NodeName, Channel) ->
    Query = case Channel of
                    stdout -> fetch_job_stdout;
                    stderr -> fetch_job_stderr
                end,
    case sqerl:select(Query, [JobId, NodeName]) of
        {ok, none} -> not_found;
        % Row might exist, but column might be null
        {ok, [[{_, null}]]} -> not_found;
        {ok, [[{_, Data}]]} -> {ok, Data}
    end.

%% Job Node Status translators
job_node_status(new) -> 0;
job_node_status(ready) -> 1;
job_node_status(running) -> 2;
job_node_status(succeeded) -> 3;
job_node_status(failed) -> 4;
job_node_status(aborted) -> 5;
job_node_status(unavailable) -> 6;
job_node_status(nacked) -> 7;
job_node_status(faulty) -> 8;
job_node_status(was_ready) -> 9;
job_node_status(crashed) -> 10;
job_node_status(timed_out) -> 11;
job_node_status(0) -> new;
job_node_status(1) -> ready;
job_node_status(2) -> running;
job_node_status(3) -> succeeded;
job_node_status(4) -> failed;
job_node_status(5) -> aborted;
job_node_status(6) -> unavailable;
job_node_status(7) -> nacked;
job_node_status(8) -> faulty;
job_node_status(9) -> was_ready;
job_node_status(10) -> crashed;
job_node_status(11) -> timed_out.

null_get(Key, Proplist) -> null_get(Key, Proplist, undefined).

null_get(Key, Proplist, Default) ->
    case safe_get(Key, Proplist) of
        null -> Default;
        Other -> Other
    end.

%% CHEF_DB CARGO_CULT
%% chef_sql:flatten_record/1
flatten_record(Rec) ->
    [_Head|Tail] = tuple_to_list(Rec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, Rec});
        false -> ok
    end,
    Tail.

%% CHEF_DB CARGO_CULT
%% chef_sql:is_undefined/1
is_undefined(undefined) ->
    true;
is_undefined(_) ->
    false.

%% CHEF_DB CARGO_CULT
parse_error({error,                      % error from sqerl
             {error,                     % error record marker from epgsql
              error,                     % Severity
              Code, Message, _Extra}}) ->
    %% See http://www.postgresql.org/docs/current/static/errcodes-appendix.html
    case Code of
        <<"23505">> -> % unique constraint violation
            {conflict, Message};
        _ ->
            {error, Message}
    end;
%% TODO: we don't actually handle these errors anywhere...
parse_error(no_connections) ->
    catch throw(no_connections), % hack to capture the stack
    error_logger:info_msg("Pooler had no connections for me, oh woe!~n~p~n", [erlang:get_stacktrace()] ),
    {error, "Pooler out of connections"};
parse_error(no_members) ->
    catch throw(no_members), % hack to capture the stack
    error_logger:info_msg("Pooler had no members for me, oh woe!~n~p~n", [erlang:get_stacktrace()]),
    {error, "Pooler had no members"}.


%% CHEF_DB CARGO_CULT
%% chef_sql:do_update/2
do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        {error, Error} -> {error, Error}
    end.

%% CHEF_DB CARGO_CULT
%% chef_sql:safe_get/2

%% @doc Safely retrieves a value from a proplist. Throws an error if the specified key does
%% not exist in the list.
-spec safe_get(Key::binary(), proplists:proplist()) -> term().
safe_get(Key, Proplist) ->
    {Key, Value} = lists:keyfind(Key, 1, Proplist),
    Value.

%% CHEF_DB CARGO_CULT
statements() ->
    File = "pgsql_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "apps", "pushy", "priv", File]),
    Rv = case file:consult(Path) of
             {ok, Statements} -> Statements;
             {error, Error} ->
                 lager:error("Cannot load statements from ~s, ~s", [File, Error]),
                 exit(no_statement_file)
         end,
    Rv.
