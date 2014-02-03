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
-type id() :: binary().
%% object ids are always 32 characters hex. This spec matches the
%% length, might be able to constrain further for range of elements.
-type object_id() :: <<_:256>>.

%% We use this to track when the pushy server itself updates DB records,
%% for example on job crash
-define(PUSHY_ACTOR_ID, <<"00000000000000000000000000000000">>).

%% job status
-type job_status() :: new |
                      voting |
                      running |
                      complete |
                      quorum_failed |
                      crashed |
                      aborted |
                      timed_out.

-type job_node_status() :: new |
                           ready |
                           running |
                           succeeded |
                           failed |
                           aborted |
                           unavailable |
                           nacked |
                           crashed |
                           was_ready |
                           timed_out.

-record(pushy_job_node, {'job_id'::object_id(),              % guid for object (unique)
                         'org_id'::object_id(),              % organization guid
                         'node_name'::binary(),              % node name
                         'status'::job_node_status(),        % node's status in context of job
                         'created_at'::binary(),  % time created at
                         'updated_at'::binary()   % time updated at
                         }).

-record(pushy_job, {'id'::object_id(),                  % guid for object (unique)
                    'org_id'::object_id(),              % organization guid
                    'command'::binary(),                % command to execute
                    'quorum'::non_neg_integer(),        % quorum count
                    'status'::job_status(),             % job status
                    'run_timeout'::non_neg_integer(),   % max duration (in seconds) to allow execution
                    'job_nodes' = [] ::[#pushy_job_node{}],
                    'last_updated_by'::object_id(),     % authz guid of last actor to update
                    'created_at'::binary(),  % time created at
                    'updated_at'::binary()  % time updated at
                    }).

-type pushy_object() :: #pushy_job{}.
