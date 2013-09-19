class RecreateCallFlows < ActiveRecord::Migration
  def up
    CallFlow.find_each do |call_flow|
      begin
        call_flow.user_flow_will_change!
        call_flow.save!
      rescue Exception => ex
        puts "Error while recreating flow #{call_flow.id}: #{ex.message}"
      end
    end
  end

  def down
  end
end
