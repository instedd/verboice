class AddKindToChannel < ActiveRecord::Migration
  def self.up
    add_column :channels, :kind, :string
  end

  def self.down
    remove_column :channels, :kind
  end
end
