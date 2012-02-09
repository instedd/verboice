class RenameQueueIdToCallQueueIdInQueuedCall < ActiveRecord::Migration
  def change
    rename_column :queued_calls, :queue_id, :call_queue_id
  end
end
