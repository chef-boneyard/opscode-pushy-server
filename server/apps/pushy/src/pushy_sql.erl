-module(pushy_sql).

%-export([fetch_node/2,
         %fetch_nodes/1,
         %fetch_nodes/2,
         %bulk_get_nodes/1,
         %create_node/1,
         %update_node/1,

         %ping/0
        %]).

-include_lib("sqerl/include/sqerl.hrl").
-include_lib("chef_common/include/chef_types.hrl").
-include_lib("chef_common/include/chef_sql.hrl").

%-spec ping() -> pong | pang.
%ping() ->
    %try
        %case sqerl:select(ping, [], rows_as_scalars, [ping]) of
            %{ok, [<<"pong">>]} -> pong;
            %_Else -> throw(pang)
        %end
    %catch
        %How:Why ->
            %error_logger:error_report({chef_sql, ping, How, Why}),
            %pang
    %end.

%%% node ops

%-spec fetch_node(bin_or_string(), bin_or_string()) -> {ok, #chef_node{} | not_found} | {error, term()}.
%fetch_node(OrgId, NodeName) ->
    %fetch_object(OrgId, NodeName, chef_node).

%-spec fetch_nodes(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%%% @doc Return list of node names for a given organization
%fetch_nodes(OrgId) ->
    %fetch_objects(OrgId, chef_node).

%-spec fetch_nodes(bin_or_string(), bin_or_string()) -> {ok, [binary()] | not_found} |
                                                       %{error, term()}.
%%% @doc Return list of node names for given organization and environment
%fetch_nodes(OrgId, EnvName) ->
    %case sqerl:select(list_env_nodes_for_org, [OrgId, EnvName], rows_as_scalars, [name]) of
        %{ok, L} when is_list(L) ->
            %{ok, L};
        %{ok, none} ->
            %{ok, []};
        %{error, Error} ->
            %{error, Error}
    %end.

%-spec create_node(#chef_node{}) -> {ok, 1} | {error, term()}.
%create_node(#chef_node{}=Node) ->
    %create_object(Node).

%-spec update_node(#chef_node{}) -> {ok, 1 | not_found} | {error, term()}.
%update_node(#chef_node{environment = Environment,
                       %last_updated_by = LastUpdatedBy,
                       %updated_at = UpdatedAt,
                       %serialized_object = Object,
                       %id = Id}) ->
    %UpdateFields = [Environment, LastUpdatedBy, UpdatedAt, Object, Id],
    %do_update(update_node_by_id, UpdateFields).

%-spec bulk_get_nodes([binary()]) -> {ok, [binary()] | not_found} |
                                    %{error, term()}.
%bulk_get_nodes(Ids) ->
    %bulk_get_objects(node, Ids).

%%% private functions

%-spec fetch_object(bin_or_string(), bin_or_string(), chef_object_name()) ->
                          %{ok, chef_object() | not_found} | {error, term()}.
%%% @doc Fetch a single Chef object with the specified `Name'. The type of object to retrieve
%%% is determined by `RecordName'.
%fetch_object(OrgId, Name, RecordName) ->
    %{QueryName, FirstRecordTxfm} = query_and_txfm_for_record(fetch, RecordName),
    %case sqerl:select(QueryName, [OrgId, Name], FirstRecordTxfm) of
        %%% Awkward sanity check that we got back the expected record type here.
        %{ok, Object} when RecordName =:= element(1, Object) ->
            %{ok, Object};
        %{ok, none} ->
            %{ok, not_found};
        %{error, Error} ->
            %{error, Error}
    %end.

%%% @doc Given a query type and chef_object() record name, return the appropriate prepared
%%% query name and sqerl record transform data to use in a call to sqerl.
%query_and_txfm_for_record(fetch, chef_node) ->
    %{find_node_by_orgid_name, ?FIRST(chef_node)};
%query_and_txfm_for_record(fetch, chef_role) ->
    %{find_role_by_orgid_name, ?FIRST(chef_role)};
%query_and_txfm_for_record(fetch, chef_environment) ->
    %{find_environment_by_orgid_name, ?FIRST(chef_environment)};
%query_and_txfm_for_record(fetch, chef_data_bag) ->
    %{find_data_bag_by_orgid_name, ?FIRST(chef_data_bag)};
%query_and_txfm_for_record(fetch, chef_data_bag_item) ->
    %{find_data_bag_item_by_orgid_name, ?FIRST(chef_data_bag_item)}.

%-spec fetch_objects(bin_or_string(), chef_object_name()) ->
                           %{ok, [binary()]} | {error, term()}.
%%% @doc Return list of object names for a given organization and object type
%fetch_objects(OrgId, RecordName) ->
    %QueryName = list_query_for(RecordName),
    %case sqerl:select(QueryName, [OrgId], rows_as_scalars, [name]) of
        %{ok, L} when is_list(L) ->
            %{ok, L};
        %{ok, none} ->
            %{ok, []};
        %{error, Error} ->
            %{error, Error}
    %end.

%list_query_for(chef_node) ->
    %list_nodes_for_org;
%list_query_for(chef_role) ->
    %list_roles_for_org;
%list_query_for(chef_environment) ->
    %list_env_nodes_for_org;
%list_query_for(chef_data_bag) ->
    %list_data_bags_for_org;
%list_query_for(chef_data_bag_item) ->
    %list_data_bag_items_for_data_bag.

%-spec bulk_get_objects(chef_type(),
                       %[binary()]) ->
                              %{ok, [binary()] | not_found} |
                              %{error, term()}.
%bulk_get_objects(Type, Ids) ->
    %Query = bulk_get_query_for_count(Type, length(Ids)),
    %case sqerl:select(Query, Ids, rows_as_scalars, [serialized_object]) of
        %{ok, none} ->
            %{ok, not_found};
        %{ok, L} when is_list(L) ->
            %{ok, L};
        %{error, Error} ->
            %{error, Error}
    %end.

%-spec create_object(chef_object()) ->
                           %{ok, non_neg_integer()} | {error, term()}.
%%% @doc create an object given a chef object record
%create_object(#chef_node{}=Node) ->
    %create_object(insert_node, Node);
%create_object(#chef_role{}=Role) ->
    %create_object(insert_role, Role);
%create_object(#chef_environment{}=Environment) ->
    %create_object(insert_environment, Environment);
%create_object(#chef_data_bag{}=DataBag) ->
    %create_object(insert_data_bag, DataBag);
%create_object(#chef_data_bag_item{}=DataBagItem) ->
    %create_object(insert_data_bag_item, DataBagItem).

%-spec create_object(atom(), tuple()) -> {ok, non_neg_integer()} | {error, term()}.
%create_object(QueryName, Record) when is_atom(QueryName) ->
    %case sqerl:statement(QueryName, flatten_record(Record), count) of
        %{ok, N} ->
            %{ok, N};
        %{error, Reason} ->
            %{ok, DbType} = application:get_env(sqerl, db_type),
            %parse_error(DbType, Reason)
        %%% FIXME: original code for create_node had the following match, but seems like
        %%% crashing would be better if we get an unexpected error.
        %%% Error -> Error
    %end.

%flatten_record(Rec) ->
    %[_Head|Tail] = tuple_to_list(Rec),
    %Tail.

%%% @doc Render the name of the bulk get query according to the bulk_get naming convention
%%% (RFC 56667)
%bulk_get_query_for_count(Type, X) ->
    %TypeStr = atom_to_list(Type) ++ "s_",
    %list_to_existing_atom("bulk_get_" ++ TypeStr ++ integer_to_list(X)).

%-spec parse_error(mysql | pgsql, string() | {error, {error, error, _, _, _}}) ->
                         %{'conflict',_} | {'error',_}.
%%% Utility for generating specific message tuples from database-specific
%%% error messages
%parse_error(mysql, Reason) ->
    %case string:str(Reason, "Duplicate entry") of
        %0 ->
            %{error, Reason};
        %_ ->
            %{conflict, Reason}
    %end;
%parse_error(pgsql, {error,                      % error from sqerl
                    %{error,                     % error record marker from epgsql
                     %error,                     % Severity
                     %Code, Message, _Extra}}) ->
    %%% See http://www.postgresql.org/docs/current/static/errcodes-appendix.html
    %case Code of
        %<<"23505">> -> % unique constraint violation
            %{conflict, Message};
        %_ ->
            %{error, Message}
    %end.

%do_update(QueryName, UpdateFields) ->
    %case sqerl:statement(QueryName, UpdateFields) of
        %{ok, 1} -> {ok, 1};
        %{ok, none} -> {ok, not_found};
        %{error, Error} -> {error, Error}
    %end.

