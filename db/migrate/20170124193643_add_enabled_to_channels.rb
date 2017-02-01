class AddEnabledToChannels < ActiveRecord::Migration
  def change
    add_column :channels, :enabled, :boolean, default: true
  end
end
