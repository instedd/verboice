class FixUserFlowBranchStepIDs < ActiveRecord::Migration
  def up
    connection.select_rows("SELECT id, user_flow FROM call_flows WHERE user_flow IS NOT NULL AND user_flow <> ''").each do |call_flow_id, user_flow|
      begin
        data = Marshal.load(Zlib.inflate(user_flow))
        next if data.nil?
        puts "Processing call flow #{call_flow_id}"
        data.select{|step| step['type'] == 'branch'}.each do |step|
          step['options'].each do |option|
            option['conditions'].each do |condition|
              referred_step = condition['step']
              next if referred_step.blank?
              puts " Fixing condition for step #{step['name']} referring to step #{referred_step}"
              condition['step'] = referred_step.to_i
            end if option['conditions']
          end if step['options']
        end
        binary_user_flow = Zlib.deflate(Marshal.dump(data), 9)
        connection.execute("UPDATE call_flows SET user_flow=x'#{binary_user_flow.unpack("H*")[0]}' WHERE call_flows.id = #{call_flow_id}")
      rescue Exception => ex
        puts "Exception fixing call flow #{call_flow_id}: #{ex.message}\n#{ex.backtrace.join("\n")}"
        raise
      end
    end
  end

  def down
  end
end
