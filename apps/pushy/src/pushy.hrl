%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2011-2012 Opscode Inc.


-record(pushy_state, {
          ctx :: any(),
          incarnation_id :: binary()
         }).
