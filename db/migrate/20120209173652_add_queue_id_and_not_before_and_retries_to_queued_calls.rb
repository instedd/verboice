class AddQueueIdAndNotBeforeAndRetriesToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :queue_id, :integer
    add_column :queued_calls, :not_before, :datetime
    add_column :queued_calls, :retries, :integer, :default => 0
  end
end
