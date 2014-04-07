class ChangePermissionsFlagsToRole < ActiveRecord::Migration
  def up
    rename_column :permissions, :flags, :role
    change_column :permissions, :role, :string
  end

  def down
    change_column :permissions, :role, :integer
    rename_column :permissions, :role, :flags
  end
end
