-module(pushy_sql).

-include_lib("pushy_sql.hrl").

-export([
         %% node status ops
         fetch_node_statuses/1,
         create_node_status/1,
         update_node_status/1,
         %% job ops
         fetch_job/1,
         create_job/1,
         %update_job/1,
         %update_job_node/1,

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
      {ok, Response};
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
    UpdateFields = [Status, LastUpdatedBy, UpdatedAt, OrgId, NodeName],
    do_update(update_node_status_by_orgid_name, UpdateFields).

%% job ops

fetch_job(JobId) ->
    case sqerl:select(find_job_by_id, [JobId]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, job_join_rows_to_record(Rows)};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

create_job(#pushy_job{} = Job) ->
    create_object(Job).

-spec create_object(Object :: pushy_object()) -> {ok, non_neg_integer()} |
                                                           {error, term()}.
%% @doc create an object given a pushy object record
create_object(#pushy_node_status{}=NodeStatus) ->
    create_object(insert_node_status, NodeStatus);
%% This does not exactly follow the same pattern as it needs to
%% insert a list of job_nodes into a separate table.
create_object(#pushy_job{id = JobId,
                          org_id = OrgId,
                          created_at = CreatedAt,
                          updated_at = UpdatedAt,
                          job_nodes = JobNodes}=Job) ->
    %% We're not dispatching to the general create_object/2 because creating a job
    %% involves adding multiple rows to multiple tables. Also, we currently embed a list of
    %% job nodes in the job record; this won't play nicely with the general
    %% 'create_object' logic, which passes all record fields (in order) as parameters for a
    %% prepared statement
    case create_object(insert_job, Job) of
      ok ->
        case insert_job_nodes(JobNodes, OrgId, JobId, CreatedAt, UpdatedAt) of
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

-spec create_object(atom(), tuple() | list()) -> {ok, non_neg_integer()} | {error, term()}.
create_object(QueryName, Record) when is_atom(QueryName) ->
    case sqerl:statement(QueryName, flatten_record(Record), count) of
        {ok, N} ->
            {ok, N};
        {error, Reason} ->
            {ok, DbType} = application:get_env(sqerl, db_type),
            parse_error(DbType, Reason)
        %% FIXME: original code for create_node had the following match, but seems like
        %% crashing would be better if we get an unexpected error.
        %% Error -> Error
    end.

%% @doc Inserts job_nodes records into the database. All records are timestamped
%% with the same stamp, namely `CreatedAt`, which is a binary string in SQL date time
%% format.
%%
%% Returns 'ok' if all records are inserted without issue. Returns an error tuple on the
%% first checksum that fails to insert into the database for whatever reason. Further
%% processing of the list is abandoned at that point.
insert_job_nodes([], _OrgId, _JobId, _CreatedAt, _UpdatedAt) ->
    ok;
insert_job_nodes([{NodeName, Status, _CreatedAt, _UpdatedAt}|Rest], OrgId, JobId, CreatedAt, UpdatedAt) ->
    case sqerl:statement(insert_job_node, [OrgId, NodeName, Status, CreatedAt, UpdatedAt, JobId], count) of
        {ok, 1} ->
            insert_job_nodes(Rest, OrgId, JobId, CreatedAt, UpdatedAt);
        {error, Reason} ->
            {error, Reason}
    end.

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
                  status = safe_get(<<"status">>, LastRow),
                  last_updated_by = safe_get(<<"last_updated_by">>, LastRow),
                  created_at = safe_get(<<"created_at">>, LastRow),
                  updated_at = safe_get(<<"updated_at">>, LastRow),
                  job_nodes = lists:reverse([C|JobNodes])};
job_join_rows_to_record([Row|Rest], JobNodes ) ->
    C = proplist_to_job_node(Row),
    job_join_rows_to_record(Rest, [C|JobNodes]).

%% @doc Convenience function for assembling a job_node tuple from a proplist
proplist_to_job_node(Proplist) ->
    {safe_get(<<"node_name">>, Proplist),
     safe_get(<<"status">>, Proplist),
     safe_get(<<"created_at">>, Proplist),
     safe_get(<<"updated_at">>, Proplist)
     }.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:flatten_record/1
flatten_record(Rec) ->
    [_Head|Tail] = tuple_to_list(Rec),
    Tail.

%% CHEF_COMMON CARGO_CULT
%% chef_sql:parse_error/1
parse_error(Reason) ->
    {ok, DbType} = application:get_env(sqerl, db_type),
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
    end.

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
    {ok, Statements} = file:consult(Path),
    Statements.
