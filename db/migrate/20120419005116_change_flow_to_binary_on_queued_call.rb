class ChangeFlowToBinaryOnQueuedCall < ActiveRecord::Migration
  def up
    change_column :queued_calls, :flow, :binary
  end

  def down
    change_column :queued_calls, :flow, :text
  end
end
