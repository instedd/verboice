class RecreateCallFlows < ActiveRecord::Migration
  def up
    CallFlow.find_each do |call_flow|
      call_flow.user_flow_will_change!
      call_flow.save!
    end
  end

  def down
  end
end
