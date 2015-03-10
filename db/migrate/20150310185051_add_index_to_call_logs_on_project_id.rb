class AddIndexToCallLogsOnProjectId < ActiveRecord::Migration
  def change
    add_index :call_logs, [:project_id]
  end
end
