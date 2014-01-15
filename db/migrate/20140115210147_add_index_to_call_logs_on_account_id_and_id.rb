class AddIndexToCallLogsOnAccountIdAndId < ActiveRecord::Migration
  def change
    add_index :call_logs, [:account_id, :id]
  end
end
