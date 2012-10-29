require File.expand_path('../settings', __FILE__)

Sequel.migration do
  change do

    alter_table(:jobs) do
      add_column(:quorum, Integer, {:null => false})
    end

  end
end

