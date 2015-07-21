class AddUniqueIndexForContactAddress < ActiveRecord::Migration
  def change
    add_index :contact_addresses, [:project_id, :address], unique: true
  end
end
