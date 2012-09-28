-module(pushy_sql).

-include_lib("pushy_sql.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([
         %% node status ops
         fetch_node_statuses/1,
         fetch_node_status/2,
         create_node_status/1,
         update_node_status/1,
         %% job ops
         fetch_job/1,
         fetch_jobs/1,
         create_job/1,
         update_job/1,
         update_job_node/1,

         sql_now/0,
         statements/1
        ]).

sql_now() -> calendar:now_to_universal_time(os:timestamp()).

%% node status ops

-spec fetch_node_statuses(binary() | string()) -> {ok, list()} | {error, term()}.
fetch_node_statuses(OrgId) ->
    case sqerl:select(list_node_statuses_for_org, [OrgId]) of
        {ok, none} ->
            {ok, []};
        {ok, Response} ->
          {ok, [node_status_row_to_record(Row) || Row <- Response]};
        {error, Reason} ->
          {error, Reason}
      end.

%% TODO Figure out correct spec
-spec fetch_node_status(binary() | string(), binary() | string() ) -> {ok, list()} | {error, term()}.
fetch_node_status(OrgId, Node) ->
    case sqerl:select(list_node_status_for_org_and_node, [OrgId, Node]) of
        {ok, none} ->
            {ok, []};
        {ok, Response} ->
            {ok, [node_status_row_to_record(Row) || Row <-Response] };
        {error, Reason} ->
            {error, Reason}
      end.

-spec create_node_status(#pushy_node_status{}) -> {ok, 1} | {error, term()}.
create_node_status(#pushy_node_status{}=NodeStatus) ->
    create_object(NodeStatus).

-spec update_node_status(#pushy_node_status{}) -> {ok, 1 | not_found} | {error, term()}.
update_node_status(#pushy_node_status{status = Status,
                                      last_updated_by = LastUpdatedBy,
                                      updated_at = UpdatedAt,
                                      node_name = NodeName,
                                      org_id = OrgId}) ->
    UpdateFields = [hb_status_as_int(Status), LastUpdatedBy, UpdatedAt, OrgId, NodeName],
    do_update(update_node_status_by_orgid_name, UpdateFields).

%% job ops

fetch_job(JobId) ->
    case sqerl:select(find_job_by_id, [JobId]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, job_join_rows_to_record(Rows)};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            lager:info("ERROR"),
            {error, Error}
    end.

fetch_jobs(OrgId) ->
    case sqerl:select(find_jobs_by_org, [OrgId]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, prepare_jobs(Rows)};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

create_job(#pushy_job{status = Status, job_nodes = JobNodes}=Job) ->
    %% convert status into an integer
    Job1 = Job#pushy_job{status=job_status(Status)},
    Fields0 = flatten_record(Job1),
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
                    %% (Remember, create_object/1 should return {ok, Number})
                    {ok, 1};
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

-spec update_job(#pushy_job{}) -> {ok, 1 | not_found} | {error, term()}.
update_job(#pushy_job{id = JobId,
                      status = Status,
                      last_updated_by = LastUpdatedBy,
                      updated_at = UpdatedAt}) ->
    UpdateFields = [job_status(Status), LastUpdatedBy, UpdatedAt, JobId],
    do_update(update_job_by_id, UpdateFields).

-spec update_job_node(#pushy_job_node{}) -> {ok, 1 | not_found} | {error, term()}.
update_job_node(#pushy_job_node{job_id = JobId,
                                node_name = NodeName,
                                org_id = OrgId,
                                status = Status,
                                updated_at = UpdatedAt}) ->
    UpdateFields = [job_node_status(Status), UpdatedAt, OrgId, NodeName, JobId],
    do_update(update_job_node_by_orgid_nodename_jobid, UpdateFields).

-spec create_object(Object :: pushy_object()) -> {ok, non_neg_integer()} |
                                                           {error, term()}.
%% @doc create an object given a pushy object record
create_object(#pushy_node_status{status=Status}=NodeStatus) ->
    NodeStatus1 = NodeStatus#pushy_node_status{status=hb_status_as_int(Status)},
    create_object(insert_node_status, NodeStatus1).

-spec create_object(atom(), tuple() | list()) -> {ok, non_neg_integer()} | {error, term()}.
create_object(QueryName, Args) when is_atom(QueryName), is_list(Args) ->
    case sqerl:statement(QueryName, Args, count) of
        {ok, N} ->
            {ok, N};
        {conflict, Reason} ->
            {conflict, Reason};
        {error, Reason} ->
            DbType = envy:get(sqerl, db_type, atom),
            parse_error(DbType, Reason)
        %% FIXME: original code for create_node had the following match, but seems like
        %% crashing would be better if we get an unexpected error.
        %% Error -> Error
    end;
create_object(QueryName, Record) when is_atom(QueryName), is_tuple(Record) ->
    List = flatten_record(Record),
    create_object(QueryName, List).

-spec job_fields_for_insert(CbFields:: list()) -> list().
job_fields_for_insert(CbFields) ->
   Pred = fun(Elem) ->
           not(is_list(Elem))
          end,
   lists:filter(Pred,CbFields).

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
            job_status(Status), CreatedAt, UpdatedAt], count) of
        {ok, 1} ->
            insert_job_nodes(Rest);
        {error, Reason} ->
            {error, Reason}
    end.

%% sequel returns seconds as float, while many erlang functions want integer
%% we trunc instead of rounding because if it's 59.6 we don't want to round to 60!
%% TODO: Move this fn to some common code
%%
trunc_date_time_to_second({{YY,MM,DD},{H,M,S}}) ->
    {{YY,MM,DD},{H,M, erlang:trunc(S)}}.

%% @doc Transforms the proplist representing a node_status query row into a record
%%
node_status_row_to_record(Row) ->
    #pushy_node_status{org_id = safe_get(<<"org_id">>, Row),
                       node_name = safe_get(<<"node_name">>, Row),
                       status = hb_status_as_atom(safe_get(<<"status">>, Row)),
                       last_updated_by = safe_get(<<"last_updated_by">>, Row),
                       created_at = trunc_date_time_to_second(safe_get(<<"created_at">>, Row)),
                       updated_at = trunc_date_time_to_second(safe_get(<<"updated_at">>, Row))}.

%% @doc Transforms a collection of proplists representing a job / job_nodes join query
%% result and collapses them all into a single job record. There is a row for each
%% job_node. A job_node tuple is extracted from each row; job information is extracted
%% from the final row (since it's the same in every row).
%%
%% See the 'find_job_by_id' prepared query for the row "shape".
job_join_rows_to_record(Rows) ->
    job_join_rows_to_record(Rows, []).
job_join_rows_to_record([LastRow|[]], JobNodes) ->
    C = proplist_to_job_node(LastRow),
    #pushy_job{id = safe_get(<<"id">>, LastRow),
                  org_id = safe_get(<<"org_id">>, LastRow),
                  command = safe_get(<<"command">>, LastRow),
                  status = job_status(safe_get(<<"status">>, LastRow)),
                  duration = safe_get(<<"duration">>, LastRow),
                  last_updated_by = safe_get(<<"last_updated_by">>, LastRow),
                  created_at = trunc_date_time_to_second(safe_get(<<"created_at">>, LastRow)),
                  updated_at = trunc_date_time_to_second(safe_get(<<"updated_at">>, LastRow)),
                  job_nodes = lists:flatten(lists:reverse([C|JobNodes]))};
job_join_rows_to_record([Row|Rest], JobNodes ) ->
    C = proplist_to_job_node(Row),
    job_join_rows_to_record(Rest, [C|JobNodes]).

prepare_jobs(Jobs) ->
    prepare_jobs(Jobs, []).

prepare_jobs([], Acc) ->
    Acc;
prepare_jobs([Head | Tail], Acc) ->
    CreatedAt = trunc_date_time_to_second(safe_get(<<"created_at">>, Head)),
    CreatedAtFormatted = iolist_to_binary(httpd_util:rfc1123_date(CreatedAt)),

    NewRow = [{<<"id">>, safe_get(<<"id">>, Head)},
              {<<"created_at">>, CreatedAtFormatted}],
    prepare_jobs(Tail, lists:append(Acc, NewRow)).

%% @doc Convenience function for assembling a job_node tuple from a proplist
proplist_to_job_node(Proplist) ->
    case safe_get(<<"node_name">>, Proplist) of
        null -> [];
        _ ->
            #pushy_job_node{job_id = safe_get(<<"id">>, Proplist),
                org_id = safe_get(<<"org_id">>, Proplist),
                node_name = safe_get(<<"node_name">>, Proplist),
                status = job_node_status(safe_get(<<"job_node_status">>, Proplist)),
                created_at = trunc_date_time_to_second(safe_get(<<"created_at">>, Proplist)),
                updated_at = trunc_date_time_to_second(safe_get(<<"updated_at">>, Proplist))}
    end.

%% Heartbeat Status translators
hb_status_as_int(X) when is_integer(X) -> X;
hb_status_as_int(down) -> 0;
hb_status_as_int(up) -> 1.
hb_status_as_atom(X) when is_atom(X) -> X;
hb_status_as_atom(0) -> down;
hb_status_as_atom(1) -> up.

%% Job Status translators
job_status(voting) -> 0;
job_status(running) -> 1;
job_status(complete) -> 2;
job_status(quorum_failed) -> 3;
job_status(aborted) -> 4;
job_status(new) -> 5;
job_status(0) -> voting;
job_status(1) -> running;
job_status(2) -> complete;
job_status(3) -> quorum_failed;
job_status(4) -> aborted;
job_status(5) -> new.

%% Job Node Status translators
job_node_status(new) -> 0;
job_node_status(ready) -> 1;
job_node_status(running) -> 2;
job_node_status(complete) -> 3;
job_node_status(aborted) -> 4;
job_node_status(unavailable) -> 5;
job_node_status(nacked) -> 6;
job_node_status(faulty) -> 7;
job_node_status(was_ready) -> 8;
job_node_status(crashed) -> 9;
job_node_status(0) -> new;
job_node_status(1) -> ready;
job_node_status(2) -> running;
job_node_status(3) -> complete;
job_node_status(4) -> aborted;
job_node_status(5) -> unavailable;
job_node_status(6) -> nacked;
job_node_status(7) -> faulty;
job_node_status(8) -> was_ready;
job_node_status(9) -> crashed.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:flatten_record/1
flatten_record(Rec) ->
    [_Head|Tail] = tuple_to_list(Rec),
    Tail.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:parse_error/1
parse_error(Reason) ->
    DbType = envy:get(sqerl, db_type, atom),
    parse_error(DbType, Reason).
%% chef_sql:parse_error/2
parse_error(mysql, Reason) ->
    case string:str(Reason, "Duplicate entry") of
        0 ->
            {error, Reason};
        _ ->
            {conflict, Reason}
    end;
parse_error(pgsql, {error,                      % error from sqerl
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
parse_error(_, no_connections) ->
    catch throw(no_connections), % hack to capture the stack
    error_logger:info_msg("Pooler had no connections for me, oh woe!~n~p~n", [erlang:get_stacktrace()] ),
    {error, "Pooler out of connections"};
parse_error(_, no_members) ->
    catch throw(no_members), % hack to capture the stack
    error_logger:info_msg("Pooler had no members for me, oh woe!~n~p~n", [erlang:get_stacktrace()]),
    {error, "Pooler had no members"}.


%% CHEF_COMMON CARGO_CULT
%% chef_sql:do_update/2
do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        {error, Error} -> {error, Error}
    end.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:safe_get/2

%% @doc Safely retrieves a value from a proplist. Throws an error if the specified key does
%% not exist in the list.
-spec safe_get(Key::binary(), Proplist::[{binary(), term()}]) -> term().
safe_get(Key, Proplist) ->
    {Key, Value} = lists:keyfind(Key, 1, Proplist),
    Value.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:statements/1
statements(DbType) ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    Rv = case file:consult(Path) of
             {ok, Statements} -> Statements;
             {error, Error} ->
                 lager:error("Cannot load statements from ~s, ~s", [File, Error]),
                 exit(no_statement_file)
         end,
    Rv.
