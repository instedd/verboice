class RemoveAddressFromContacts < ActiveRecord::Migration
  def up
    connection.execute "INSERT INTO contact_addresses (contact_id, project_id, address, created_at, updated_at) SELECT id, project_id, address, created_at, created_at FROM contacts"

    remove_column :contacts, :address
  end

  def down
    add_column :contacts, :address, :string

    connection.execute "UPDATE contacts SET address = (SELECT address FROM contact_addresses WHERE contact_addresses.contact_id = contacts.id LIMIT 1)"
  end
end
