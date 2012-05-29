%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Kevin Smith <kevin@opscode.com>
%%% @author Seth Falcon <seth@opscode.com>
%%% @copyright Copyright 2011-2012 Opscode Inc.
%%% @doc
%%% REST resource for monitoring status of erchef
%%% @end
-module(pushy_status_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(A2B(X), erlang:atom_to_binary(X, utf8)).

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
    Mods = [chef_otto, chef_sql, chef_authz, chef_solr],
    Pings = [ {?A2B(Mod), ?A2B(Mod:ping())} || Mod <- Mods ],
    Status = case [ Pang || {_, <<"pang">>}=Pang <- Pings ] of
                 [] -> pong;
                 _Pangs -> pang
             end,
    {Status, ejson:encode({[{<<"status">>, ?A2B(Status)}, {<<"upstreams">>, {Pings}}]})}.

