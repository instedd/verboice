class AddPbxLogsGuidToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :pbx_logs_guid, :string
  end
end
