%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-type pushy_requestor_type() :: 'client' | 'user'.

-record(pushy_principal, {requestor_id :: binary(),
                          requestor_type :: pushy_requestor_type(),
                          requestor_key :: binary()}).


-record(config_state, {organization_name :: binary(),
                       organization_guid :: <<_:256>>,
                       % TODO: probably want to split this into specific states, instead of this
                       % catch-all, but right now authentication requires the above two things,
                       % and config + job endpoints (respectively) need the following:
                       node_name :: string(),
                       % TODO: can't use #pushy_job{} here without some minor re-jiggering elsewhere;
                       % not everything that includes this includes pushy_sql.hrl
                       pushy_job :: tuple(),
                       incarnation_id :: binary(),
                       requestor :: binary(),

                       %% Authz ID of the requestor
                       requestor_id :: binary(),
                       requestor_type :: pushy_requestor_type(),
                       requestor_key :: any()
                      }).

-define(AUTH_SKEW, 900).
-define(MAX_SIZE, 1000000).
