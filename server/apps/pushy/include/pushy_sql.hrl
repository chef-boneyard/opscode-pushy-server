%% a bit odd, but field names have to match column names for helper
%% function to work.

-type id() :: binary().

-define(POC_ORG_ID, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>).
-define(POC_ORG_NAME, <<"pushy">>).
-define(POC_ACTOR_ID, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>).
-define(POC_HB_THRESHOLD, 3).

-record(pushy_node_status, {'org_id',           % organization guid
                            'node_name',        % node name
                            'status',           % node status
                            'last_updated_by',  % authz guid of last actor to update object
                            'created_at',       % time created at
                            'updated_at'        % time created at
                            }).

-type pushy_object() :: #pushy_node_status{}.
