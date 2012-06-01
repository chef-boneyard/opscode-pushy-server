-module(pushy_sql).

-include_lib("pushy_sql.hrl").

-export([
         create_node_status/1,
         update_node_status/1,
         sql_now/0,
         fetch_node_statuses/1,
         statements/1
        ]).

sql_now() -> calendar:now_to_universal_time(os:timestamp()).

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

-spec fetch_node_statuses(binary() | string()) -> {ok, list()} | {error, term()}.
fetch_node_statuses(OrgId) ->
  case sqerl:select(get_node_status_by_orgid, [OrgId]) of
    {ok, none} ->
      {ok, []};
    {ok, Response} ->
      {ok, Response};
    {error, Reason} ->
      {error, Reason}
  end.

do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        {error, Error} -> {error, Error}
    end.


%% CHEF_COMMON CARGO_CULT

%% @doc create an object given a pushy object record
create_object(#pushy_node_status{}=NodeStatus) ->
    create_object(insert_node_status, NodeStatus).

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

%% CHEF_COMMON CARGO_CULT
%% chef_sql:flatten_record/1
flatten_record(Rec) ->
    [_Head|Tail] = tuple_to_list(Rec),
    Tail.

%% CHEF_COMMON CARGO_CULT
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
%% chef_sql:statements/1
statements(DbType) ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.
