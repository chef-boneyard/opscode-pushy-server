%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Handle org_name to org_guid mappings by calling out to
%% opscode-account
-module(pushy_org).

-define(POC_ORG_ID, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>).

-export([
          fetch_org_id/1
        ]).

%% Stub it out for now
-spec fetch_org_id(OrgName :: string()) -> binary().
fetch_org_id(_OrgName) ->
  ?POC_ORG_ID.



