class RenameCallQueueToQueuedCall < ActiveRecord::Migration
  def self.up
    rename_table :call_queues, :queued_calls
  end

  def self.down
    rename_table :queued_calls, :call_queues
  end
end
