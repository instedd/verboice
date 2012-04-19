class AddGuidToChannel < ActiveRecord::Migration
  def change
    add_column :channels, :guid, :string
  end
end
