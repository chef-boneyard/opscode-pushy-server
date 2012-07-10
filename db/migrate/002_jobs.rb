require File.expand_path('../settings', __FILE__)

Sequel.migration do
  change do

    # For Pushy PoC simplicity:
    # * we are using node_name (vs node_id) with no foreign key constraint
    # * authz_id column has intentionally been left out for PoC simplicity

    create_table(:jobs) do
      String(:id, :primary_key => true, :fixed => true, :size => 32)
      String(:org_id, :null => false, :fixed => true, :size => 32)
      String(:command, :null => false)
      Integer(:status, :null => false)
      Integer(:duration, :null => true)

      String(:last_updated_by, :null => false, :fixed => true, :size => 32)
      DateTime(:created_at, :null => false)
      DateTime(:updated_at, :null => false)
    end

    create_table(:job_nodes) do
      String(:job_id, :null => :false, :fixed => true, :size => 32)
      String(:org_id, :null => false, :fixed => true, :size => 32)
      String(:node_name, :null => false)
      # String(:node_id, :null => :false, :fixed => true, :size => 32)
      Integer(:status, :null => false)

      DateTime(:created_at, :null => false)
      DateTime(:updated_at, :null => false)

      unique([:job_id, :org_id, :node_name]) # only one node with a given name/org per job
      foreign_key([:job_id], :jobs, :key => :id, :on_delete => :cascade, :on_update => :cascade)
      #foreign_key([:org_id, :node_id], :nodes, :key => [:org_id, :node_id], :on_delete => :cascade, :on_update => :cascade)
    end

  end
end
