class UpdateReferencesToCallFlow < ActiveRecord::Migration
  def up
    add_column :call_logs, :call_flow_id, :integer
    add_index :call_logs, :call_flow_id

    rename_column :channels, :project_id, :call_flow_id
    add_index :channels, :call_flow_id

    add_column :queued_calls, :call_flow_id, :integer
    add_index :queued_calls, :call_flow_id

    rename_column :traces, :project_id, :call_flow_id
    add_index :traces, :call_flow_id
  end

  def down
    remove_index :call_logs, :call_flow_id
    remove_column :call_logs, :call_flow_id

    remove_index :channels, :call_flow_id
    rename_column :channels, :call_flow_id, :project_id

    remove_index :queued_calls, :call_flow_id
    remove_column :queued_calls, :call_flow_id

    remove_index :traces, :call_flow_id
    rename_column :traces, :call_flow_id, :project_id
  end
end
