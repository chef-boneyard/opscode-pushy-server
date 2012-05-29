%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource getting config information for the push jobs
%%% @end
-module(pushy_config_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("eunit/include/eunit.hrl").

-record(config_state, {organization_guid :: string() }).

init(Config) ->
    ?debugVal(Config),
    State = #config_state{},
%%    {ok, State},
    {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace


allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    case application:get_env(pushy, configuration_json) of
        {ok, Data} -> {ejson:encode(Data), Req, State};
        _ -> error_logger:error_msg("Missing key pushy configuration_json~n", []),
             erlang:error({error, "Missing pushy configuration_json key"})
    end.


