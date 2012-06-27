require File.expand_path('../settings', __FILE__)

Sequel.migration do
  change do

    # For Pushy PoC simplicity:
    # * we are using node_name (vs node_id) with no foreign key constraint
    # * authz_id column has intentionally been left out for PoC simplicity

    create_table(:node_status) do
      String(:node_name, :null => false)
      # String(:node_id, :null => :false, :fixed => true, :size => 32)
      String(:org_id, :null => false, :fixed => true, :size => 32)
      Integer(:status, :null => false)

      String(:last_updated_by, :null => false, :fixed => true, :size => 32)
      DateTime(:created_at, :null => false)
      DateTime(:updated_at, :null => false)
      primary_key([:org_id, :node_name])
      unique([:org_id, :node_name]) # only one node with a given name in an org
      # foreign_key([:org_id, :node_id], :nodes, :key => [:org_id, :node_id], :on_delete => :cascade, :on_update => :cascade)
    end
  end
end
