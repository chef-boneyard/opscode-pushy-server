-module(pushy_sql).

-include_lib("eunit/include/eunit.hrl").

-export([create_node_status/2,
         update_node_status/2,
         statements/1
  ]).


create_node_status(Name, Status) ->
  Record = [make_id(Name), Name, Status],
  create_object(insert_node_status, Record).

update_node_status(Name, Status) ->
  UpdateFields = [Status, Name],
  do_update(update_node_status, UpdateFields).


do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        {error, Error} -> {error, Error}
    end.


create_object(QueryName, Record) when is_atom(QueryName) ->
    case sqerl:statement(QueryName, Record, count) of
        {ok, N} ->
            {ok, N};
        {error, Reason} ->
            {ok, DbType} = application:get_env(sqerl, db_type),
            parse_error(DbType, Reason)
        %% FIXME: original code for create_node had the following match, but seems like
        %% crashing would be better if we get an unexpected error.
        %% Error -> Error
    end.

make_id(Name) ->
  Bin = iolist_to_binary([Name, crypto:rand_bytes(16)]),
  <<ObjectPart:80, _/binary>> = crypto:md5(Bin),
  %% FIXME: Shits broke we need to fix it
  iolist_to_binary(io_lib:format("~p", [ObjectPart])).

%% Utility for generating specific message tuples from database-specific
%% error messages
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

statements(DbType) ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.
