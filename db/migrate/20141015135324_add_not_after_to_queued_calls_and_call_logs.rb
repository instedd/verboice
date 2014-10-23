class AddNotAfterToQueuedCallsAndCallLogs < ActiveRecord::Migration
  def change
    add_column :queued_calls, :not_after, :datetime
    add_column :call_logs, :not_after, :datetime
  end
end
