%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General messaging utilities for ZeroMQ

-record(message,
        {address :: binary() | 'none',
         version :: binary(),
         signature :: binary() | 'none',
         validated :: 'ok_sofar' | 'ok' | {'fail', any()},
         raw  :: binary(),
         body :: any() % Get a viable json type here
        }).

