%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

%% @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(pushy_event).

-export([get_time_as_iso8601/1, make_event/3]).

-include_lib("eunit/include/eunit.hrl").
-include("pushy_event.hrl").

get_time_as_iso8601(Time) ->
    {_, _, Micros} = Time,
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Time),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0BZ",
                  [YY, MM, DD, Hour, Min, Sec, Micros]). 

make_event(Name, Id, Props) ->
    #event{name = list_to_binary(Name), id = Id, timestamp = erlang:now(), data = Props}.

