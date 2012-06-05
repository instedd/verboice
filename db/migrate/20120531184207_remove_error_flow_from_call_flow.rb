class RemoveErrorFlowFromCallFlow < ActiveRecord::Migration
  def up
    remove_column :call_flows, :error_flow
  end

  def down
    add_column :call_flows, :error_flow, :binary
  end
end
