class AddAnonymousFlagToContacts < ActiveRecord::Migration
  def change
    add_column :contacts, :anonymous, :boolean
  end
end
