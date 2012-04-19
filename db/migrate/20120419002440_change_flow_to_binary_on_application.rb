class ChangeFlowToBinaryOnApplication < ActiveRecord::Migration
  def up
    change_column :applications, :flow, :binary
  end

  def down
    change_column :applications, :flow, :text
  end
end
