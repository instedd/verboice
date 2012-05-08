class AddConfirmableToAccounts < ActiveRecord::Migration
  def change
    add_column :accounts, :confirmation_token, :string
    add_column :accounts, :confirmed_at, :datetime
    add_column :accounts, :confirmation_sent_at, :datetime
    add_index :accounts, :confirmation_token, :unique => true
  end
end
