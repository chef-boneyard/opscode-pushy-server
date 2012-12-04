%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author James Casey <james@opscode.com>
%%% @copyright Copyright 2012-2012 Opscode Inc.
%%% @doc
%%% Somee helper tools
%%% @end

-module(pushy_tools).

-define(PUSHY_ORG,<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">> ).
-include("pushy_sql.hrl").

-export([send_job/3,
         bin_to_hex/1]).

%% @doc helper function to generate a job and send it to a set of clients listening
%% on a simulator.  It assumes the node names are of the form:  HOSTNAME-000ID
%% where HOSTNAME is the server where the simulated clients are running
send_job(Host, OrgName, Num) ->
    Names = [ construct_name(Host, N) || N <- lists:seq(1, Num)],
    Job = pushy_object:new_record(pushy_job, OrgName, Names, <<"chef-client">>,
                                  10000, Num),
    pushy_job_state_sup:start(Job).

%%
%% Internal functions
%%

construct_name(Hostname, Id) ->
    list_to_binary(io_lib:format("~s-~4..0B", [Hostname, Id])).

%%
%% Generate a pretty hexadecimal output for a binary. 
bin_to_hex(Bin) when is_binary(Bin) ->
    lists:flatten([ [ erlang:integer_to_list(Nibble1, 16), erlang:integer_to_list(Nibble2, 16) ]
                    || << Nibble1:4, Nibble2:4 >> <= Bin ]).
