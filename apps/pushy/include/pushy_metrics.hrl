%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Chisamore <schisamo@opscode.com>
%% @copyright 2012 Opscode Inc.

%% @doc Send timing data for Fun(Args) to the upstream folsom metric.
%%
%% NB: we require a space between Mod:Fun and Args since we are
%% abusing the text replacement and expect Args to be of the form
%% '(arg1, arg2, ..., argN)'.
%% @end
-define(TIME_IT(Mod, Fun, Args),
        pushy_metrics:ctime(pushy_metrics:label(Mod, Fun),
                         fun() -> Mod:Fun Args end)).
