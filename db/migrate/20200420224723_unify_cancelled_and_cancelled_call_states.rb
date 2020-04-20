class UnifyCancelledAndCancelledCallStates < ActiveRecord::Migration
  def up
    ActiveRecord::Base.connection.execute <<-SQL
      UPDATE `call_logs`
      SET `state` = "cancelled"
      WHERE `state` = "canceled"
    SQL
  end

  def down
    # Nothing to do - can't revert the change
  end
end
