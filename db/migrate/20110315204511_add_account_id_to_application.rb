class AddAccountIdToApplication < ActiveRecord::Migration
  def self.up
    add_column :applications, :account_id, :integer
  end

  def self.down
    remove_column :applications, :account_id
  end
end
