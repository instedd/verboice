class AddIndexToCallLogsOnState < ActiveRecord::Migration
  def change
    add_index :call_logs, [:state]
  end
end
