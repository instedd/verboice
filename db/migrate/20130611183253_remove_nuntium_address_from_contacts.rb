class RemoveNuntiumAddressFromContacts < ActiveRecord::Migration
  def up
    remove_column :contacts, :nuntium_address
  end

  def down
    add_column :contacts, :nuntium_address, :string
  end
end
