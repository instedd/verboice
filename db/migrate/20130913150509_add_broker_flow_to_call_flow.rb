class AddBrokerFlowToCallFlow < ActiveRecord::Migration
  def change
    add_column :call_flows, :broker_flow, :blob
  end
end
