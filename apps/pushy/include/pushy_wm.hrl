%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-record(config_state, {
          orgname :: string(),
          organization_guid :: string(),
          nodename :: string() }).

-define(AUTH_SKEW, 900).
-define(MAX_SIZE, 1000000).
