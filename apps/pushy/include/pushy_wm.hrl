%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>

%% @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
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
-type pushy_requestor_type() :: 'client' | 'user'.

-record(pushy_principal, {requestor_id :: binary(),
                          requestor_type :: pushy_requestor_type(),
                          requestor_key :: binary()}).

-record(config_state, {organization_name :: binary(),
                       organization_guid :: <<_:256>>,
                       % TODO: probably want to split this into specific states, instead of this
                       % catch-all, but right now authentication requires the above two things,
                       % and config + job endpoints (respectively) need the following:
                       node_name :: binary(),
                       % TODO: can't use #pushy_job{} here without some minor re-jiggering elsewhere;
                       % not everything that includes this includes pushy_sql.hrl
                       pushy_job :: tuple(),
                       incarnation_id :: binary(),
                       requestor :: binary(),
                       max_body_size :: binary(),

                       %% Authz ID of the requestor
                       %% TODO: Why isn't this just an embedded #pushy_principal{} record instead?
                       requestor_id :: binary(),
                       requestor_type :: pushy_requestor_type(),
                       requestor_key :: any(),

                       curve_public_key :: binary(),

                       % Used only by pushy_job_output_resource
                       output :: 'stdout' | 'stderr' | undefined,
                       output_data :: binary() | undefined,

                       job_pid :: pid()
                      }).

-record(chef_api_response, { response_api_version :: integer(),
                             response_code :: string(),
                             response_body :: ejson_term(),
                             response_headers :: list()
                           }).

-type ibrowse_response() :: {error, any()} | {ok, undefined | string(), [any()], binary() | maybe_improper_list()}.
-type ejson_term() :: {maybe_improper_list()}.
-type chef_api_response() :: #chef_api_response{}.



-define(AUTH_SKEW, 900).
-define(MAX_SIZE, 1000000).
-define(MAX_FILE_SIZE, 100000).
