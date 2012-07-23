%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Mark Anderson <mark@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% REST resource for monitoring status of pushy. Extracted from work done by Seth Falcon and Kevin Smith for erchef
%%% @end
-module(pushy_status_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

init(_Any) ->
    {ok, <<"{}">>}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    case check_health(Req, State) of
%        {pang, Body} ->
%            {{halt, 500}, wrq:set_resp_body(Body, Req), State};
        {pong, Body} ->
            {Body, Req, State}
    end.

%% private functions

check_health(_Req, _State) ->
    Status = {[
               {<<"status">>, <<"it's alive">> },
               {<<"node_states">>,
                {[ {atom_to_binary(S, utf8), V} || {S,V} <- pushy_counters:get_aggregate_counters()]}}
              ]},
    {pong, ejson:encode(Status)}.

