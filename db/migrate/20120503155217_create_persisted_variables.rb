class CreatePersistedVariables < ActiveRecord::Migration
  def change
    create_table :persisted_variables do |t|
      t.references :account
      t.integer :value
      t.string :name
      t.string :address

      t.timestamps
    end
    add_index :persisted_variables, :account_id
  end
end
