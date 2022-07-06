class AddCallbackurlAndContextToCallLogs < ActiveRecord::Migration
  def up
    add_column :call_logs, :callback_url, :string
    add_column :call_logs, :js_context  , :text
  end

  def down 
    remove_column :call_logs, :callback_url
    remove_column :call_logs, :js_context
  end
end
