%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General utility module for common functions that operate on
-module(pushy_util).

-export([
         make_zmq_socket_addr/1,
         make_zmq_socket_addr/2,
         make_zmq_socket_addr/3,
         get_env/3
        ]).

-include_lib("eunit/include/eunit.hrl").




make_zmq_socket_addr(Port) ->
    Host = get_env(pushy, zeromq_listen_address, fun is_list/1),
    make_zmq_socket_addr(Host, Port).

make_zmq_socket_addr(Host, PortName, tcp) ->
    ProtoHost = io_lib:format("tcp://~s", [Host]),
    make_zmq_socket_addr(ProtoHost, PortName).


make_zmq_socket_addr(Host, PortName) when is_atom(PortName) ->
    Port = get_env(pushy, PortName, fun is_integer/1),
    make_zmq_socket_addr(Host, Port);
make_zmq_socket_addr(Host, Port) when is_integer(Port) ->
    lists:flatten(io_lib:format("~s:~w",[Host,Port])).

get_env(Section, Item, TypeCheck) ->
    case application:get_env(Section, Item) of
        {ok, Value} ->
            case TypeCheck(Value) of
                true -> Value;
                Error ->
                    error_logger:error_msg("Bad typecheck for config item for ~p ~p (~p(~p) -> ~p)~n",
                                           [Section, Item, TypeCheck, Value, Error]),
                    error(config_bad_item)
            end;
        undefined ->
            error_logger:error_msg("Bad config item for ~p ~p ~n", [Section, Item]),
            error(config_missing_item)
    end.

