class AddCallbackUrlAndJsContextToCallLogs < ActiveRecord::Migration
    def change
        add_column :call_logs, :callback_url, :string, default: ""
        add_column :call_logs, :js_context, :text, default: ""
    end
  end
  