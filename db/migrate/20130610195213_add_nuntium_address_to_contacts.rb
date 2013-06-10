class AddNuntiumAddressToContacts < ActiveRecord::Migration
  def change
    add_column :contacts, :nuntium_address, :string
  end
end
