%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc General messaging utilities for ZeroMQ
-type json_term() :: any().

-record(message,
        {validated :: 'ok_sofar' | 'ok' | {'fail', any()},
         id :: reference(),
         address :: binary() | 'none',
         header  :: binary() | 'none',
         raw  :: binary() | 'none',

         version :: binary(),
         signature :: binary() | 'none',

         body :: json_term() % Get a viable json type here
        }).
