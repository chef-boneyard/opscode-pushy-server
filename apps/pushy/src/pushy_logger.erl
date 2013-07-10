%%%-------------------------------------------------------------------
%%% @author Douglas Triggs
%%% @copyright 2013 Opscode, Inc.
%%% @doc A simple wrapper for getting logger access all into one place
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pushy_logger).

-export([debug/1,
         debug/2,
         debug/3,
         error/1,
         error/2,
         info/1,
         info/2,
         trace_file/2,
         warn/1,
         warn/2]).

-compile([{parse_transform, lager_transform}]).

trace_file(File, Params) ->
    lager:trace_file(File, Params).

debug(Message) ->
    lager:debug(Message).

debug(Message, Data) ->
    lager:debug(Message, Data).

debug(Context, Message, Data) ->
    lager:debug(Context, Message, Data).

error(Message) ->
    lager:error(Message).

error(Message, Data) ->
    lager:error(Message, Data).

info(Message) ->
    lager:info(Message).

info(Message, Data) ->
    lager:info(Message, Data).

warn(Message) ->
    lager:warn(Message).

warn(Message, Data) ->
    lager:warn(Message, Data).
