class AddCallQueueIdAndNotBeforeToCallLog < ActiveRecord::Migration
  def change
    add_column :call_logs, :call_queue_id, :integer
    add_column :call_logs, :not_before, :datetime
  end
end
