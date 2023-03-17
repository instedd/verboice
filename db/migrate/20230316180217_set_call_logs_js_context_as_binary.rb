class SetCallLogsJsContextAsBinary < ActiveRecord::Migration
  def up
    connection.execute "UPDATE call_logs SET js_context = NULL WHERE js_context IS NOT NULL"
    change_column :call_logs, :js_context, :binary
  end

  def down
    connection.execute "UPDATE call_logs SET js_context = NULL WHERE js_context IS NOT NULL"
    change_column :call_logs, :js_context, :text
  end
end
