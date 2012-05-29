-type db_type() :: sqerl:db_type().

%% object ids are always 32 characters hex. This spec matches the
%% length, might be able to constrain further for range of elements.
-type object_id() :: <<_:256>>.

-type bin_or_string() :: binary() | string().

-type chef_object_name() :: chef_data_bag |
                            chef_data_bag_item |
                            chef_environment |
                            chef_role |
                            chef_node.

-type chef_type() :: % 'cookbook' |
                     'data_bag' |
                     'data_bag_item' |
                     'environment' |
                     'node' |
                     'role'.

-type ejson_term() :: {maybe_improper_list()}.

