class CreateContactAddresses < ActiveRecord::Migration
  def change
    create_table :contact_addresses do |t|
      t.string :address
      t.references :contact
      t.references :project

      t.timestamps
    end
    add_index :contact_addresses, :contact_id
    add_index :contact_addresses, :project_id
  end
end
