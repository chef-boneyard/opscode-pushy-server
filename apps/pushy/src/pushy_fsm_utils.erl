

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
-module(pushy_fsm_utils).

-export([safe_sync_send_all_state_event/2,
         intersperse/2
        ]).


%%
%% API
%%

%% We can end up in a race condition with sync messages where
%% the process terminates and the message is still in the queue
%%
%% This deals with the race condition by matching the error
%% message returned and converting it to `undefined`.
-spec safe_sync_send_all_state_event(Pid :: pid(), Message :: term()) -> not_found | term().
safe_sync_send_all_state_event(Pid, Message) ->
    case catch gen_fsm:sync_send_all_state_event(Pid, Message) of
        {'EXIT', {shutdown, _Details}} ->
            not_found;
        Else ->
            Else
    end.

%% Some list functions
prependToAll(_, []) -> [];
prependToAll(E, [X|Xs]) -> [E|[X|prependToAll(E,Xs)]].

intersperse(_, []) -> [];
intersperse(E, [X|Xs]) -> [X|prependToAll(E,Xs)].

% Note -- lists:concat doesn't work if there are binaries in the list!
%intercalate(Xs, Xss) -> lists:concat(intersperse(Xs, Xss)).

