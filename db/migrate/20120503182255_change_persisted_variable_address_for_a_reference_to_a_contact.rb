class ChangePersistedVariableAddressForAReferenceToAContact < ActiveRecord::Migration
  def up
    remove_column :persisted_variables, :address
    remove_column :persisted_variables, :account_id
    add_column :persisted_variables, :contact_id, :integer
    add_index :persisted_variables, :contact_id
    change_column :persisted_variables, :value, :string
  end

  def down
    add_column :persisted_variables, :address, :string
    add_column :persisted_variables, :account_id, :integer
    remove_column :persisted_variables, :contact_id
    change_column :persisted_variables, :value, :integer
    add_index :persisted_variables, :account_id
  end
end
