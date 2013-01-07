

-module(pushy_fsm_utils).

-export([safe_sync_send_all_state_event/2
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
