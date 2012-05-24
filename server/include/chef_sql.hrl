%% a bit odd, but field names have to match column names for helper
%% function to work.

-type id() :: binary().

-define(KEY_VERSION, 0).
-define(CERT_VERSION, 1).

%% Currently only used by chef_authz
-record(chef_container, {'id',             % guid for object (unique)
                         'authz_id',       % authorization guid (unique)
                         'org_id',         % organization guid
                         'name',           % name of container
                         'path',           % 'path' of container (not used? Orig part of inheritance mech?; safe to delete? Yea!)
                         'last_updated_by' % authz guid of last actor to update object
                        }).

%% currently only used by chef_authz; no SQL support yet
%% Possible extra fields include created_at and updated_at (or not)
-record(chef_client, {'id',              % guid for object (unique)
                      'authz_id',        % authorization guid (unique)
                      'org_id',          % organization guid
                      'name',            % name of client
                      'validator',       % boolean; true if this is a validator
                      %% FIXME: do we want to rename this to match the SQL schema?
                      %% If so, need to update authorization code paths.
                      'certificate',     % public key cert
                      'pubkey_version',  % version/type of publick key (certificate)
                      'last_updated_by', % authz guid of last actor to update object
                      'created_at',      % time created at
                      'updated_at'       % time created at
                     }).

-record(chef_data_bag, {'id',               % guid for object (unique)
                        'authz_id',         % authorization guid (unique)
                        'org_id',           % organization guid
                        'name',             % data_bag name
                        'last_updated_by',  % authz guid of last actor to update object
                        'created_at',       % time created at
                        'updated_at'       % time created at
                   }).

-record(chef_data_bag_item, {'id',               % guid for object (unique)
                                                 %% right now authz for items is done via the parent data_bag
                                                 %% 'authz_id',         % authorization guid (unique)
                             'org_id',           % organization guid
                             'data_bag_name',    % parent data_bag name
                             'item_name',        % data_bag_item name
                             'last_updated_by',  % authz guid of last actor to update object
                             'created_at',       % time created at
                             'updated_at',       % time created at
                             'serialized_object' % json blob of object data
                            }).

-record(chef_environment, {'id',               % guid for object (unique)
                           'authz_id',         % authorization guid (unique)
                           'org_id',           % organization guid
                           'name',             % environment name
                           'last_updated_by',  % authz guid of last actor to update object
                           'created_at',       % time created at
                           'updated_at',       % time created at
                           'serialized_object' % json blob of object data
                          }).

-record(chef_node, {'id',               % guid for object (unique)
                    'authz_id',         % authorization guid (unique)
                    'org_id',           % organization guid
                    'name',             % node name
                    'environment',      % environment
                    'last_updated_by',  % authz guid of last actor to update object
                    'created_at',       % time created at
                    'updated_at',       % time created at
                    'serialized_object' % json blob of object data
                   }).

%% This doesn't quite belong here, but rather in a chef_db hrl file.
%% Used as a common data format for actor data (users or clients).
-record(chef_requestor, {
          type = user :: 'user' | 'client',
          authz_id,
          name,
          key_data}).

-record(chef_role, {'id',               % guid for object (unique)
                    'authz_id',         % authorization guid (unique)
                    'org_id',           % organization guid
                    'name',             % role name
                    'last_updated_by',  % authz guid of last actor to update object
                    'created_at',       % time created at
                    'updated_at',       % time created at
                    'serialized_object' % json blob of object data
                   }).

-record(chef_user, {'id',
                    'authz_id',
                    'username',
                    'pubkey_version',
                    'public_key'}).

-type chef_object() :: #chef_data_bag{} |
                       #chef_data_bag_item{} |
                       #chef_environment{} |
                       #chef_role{} |
                       #chef_node{}.

-type chef_indexable_object() :: #chef_data_bag_item{} |
                                 #chef_environment{} |
                                 #chef_role{} |
                                 #chef_node{}.

