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
-type node_name() :: binary().
-type org_id() :: binary().
-type node_ref() :: {org_id(), node_name()}.
-type node_addr() :: binary().
-type job_event() :: ack_commit | nack_commit | ack_run | nack_run | succeeded | failed | aborted.
-type node_event() :: heartbeat | job_event().

% node status
-type node_status() :: online |
                       offline.

% node availablity
-type node_availability() :: available |
                             unavailable.

-type incarnation_id() :: binary().

%% TODO: this seems as though it could be tightened up
%%
%% TODO: The binary expression is the same as object_id() in
%% pushy_sql.hrl.  We need to do a bit of type refactoring so that we
%% can directly use that type here, instead of duplicating.
-type job_id() :: <<_:256>> | null | invalid_job_id.
