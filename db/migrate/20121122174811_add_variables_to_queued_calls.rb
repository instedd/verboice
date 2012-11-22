class AddVariablesToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :variables, :text
  end
end
