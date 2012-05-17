class RenameCallQueuesWithSchedules < ActiveRecord::Migration
  def change
      rename_table :call_queues, :schedules
      rename_column :queued_calls, :call_queue_id, :schedule_id
      rename_column :call_logs, :call_queue_id, :schedule_id
  end
end
