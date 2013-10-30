class AddIndexToQueuedCallsOnCallLogId < ActiveRecord::Migration
  def change
    add_index :queued_calls, [:call_log_id]
  end
end
