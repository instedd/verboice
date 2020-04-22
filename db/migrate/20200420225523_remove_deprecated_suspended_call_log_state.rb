class RemoveDeprecatedSuspendedCallLogState < ActiveRecord::Migration
  def up
    ActiveRecord::Base.connection.execute <<-SQL
      UPDATE `call_logs`
      SET `state` = "cancelled", `fail_reason` = "Call was kept in a deprecated 'suspended' state"
      WHERE `state` = "suspended"
    SQL
  end

  def down
    # Nothing to do - can't revert the change
  end
end
