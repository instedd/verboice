class AddSessionIdToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :session_id, :string
  end
end
