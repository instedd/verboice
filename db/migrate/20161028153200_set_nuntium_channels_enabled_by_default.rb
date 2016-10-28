class SetNuntiumChannelsEnabledByDefault < ActiveRecord::Migration
  def up
    change_column_default :nuntium_channels, :enabled, true
  end

  def down
    change_column_default :nuntium_channels, :enabled, nil
  end
end
