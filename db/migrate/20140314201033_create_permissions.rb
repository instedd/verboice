class CreatePermissions < ActiveRecord::Migration
  def change
    create_table :permissions do |t|
      t.integer :account_id
      t.string :type
      t.integer :model_id
      t.integer :flags

      t.timestamps
    end
  end
end
