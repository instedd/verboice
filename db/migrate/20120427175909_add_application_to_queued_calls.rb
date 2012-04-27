class AddApplicationToQueuedCalls < ActiveRecord::Migration

  def up
    add_column :queued_calls, :application_id, :integer
    add_index "queued_calls", ["application_id"], :name => "index_queued_calls_on_application_id"
  end

  def down
    remove_column :queued_calls, :application_id
  end
end
