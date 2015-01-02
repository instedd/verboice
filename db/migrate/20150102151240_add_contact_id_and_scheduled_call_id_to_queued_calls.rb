class AddContactIdAndScheduledCallIdToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :contact_id, :integer
    add_column :queued_calls, :scheduled_call_id, :integer
  end
end
