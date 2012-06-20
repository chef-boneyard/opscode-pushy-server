%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% TODO - find a better spot for this log setup
    % Logs all job message to a specific file
    lager:trace_file("log/jobs.log", [{job_id, '*'}]),

    case erlzmq:context() of
        {ok, Ctx} ->
            case pushy_sup:start_link(Ctx) of
                {ok, Pid} -> {ok, Pid, Ctx};
                Error -> Error
            end;
        Error ->
            Error
    end.

stop(Ctx) ->
    erlzmq:term(Ctx, 5000).
