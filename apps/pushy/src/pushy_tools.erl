%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author James Casey <james@opscode.com>
%%% @doc
%%% Somee helper tools
%%% @end

%% @copyright Copyright 2012-2012 Chef Software, Inc. All Rights Reserved.
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
-module(pushy_tools).

-include("pushy_sql.hrl").

-export([send_job/3,
         bin_to_hex/1]).

%% @doc helper function to generate a job and send it to a set of clients listening
%% on a simulator.  It assumes the node names are of the form:  HOSTNAME-000ID
%% where HOSTNAME is the server where the simulated clients are running
send_job(Host, OrgName, Num) ->
    Names = [ construct_name(Host, N) || N <- lists:seq(1, Num)],
    OrgId = pushy_org:fetch_org_id(OrgName),
    Job = pushy_object:new_record(pushy_job, OrgId, Names, <<"chef-client">>,
                                  10000, Num, <<"chef-user">>),
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
