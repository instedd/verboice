class AddSessionVarsToCallLogs < ActiveRecord::Migration
  def up
    change_table :call_logs do |t|
      t.remove :callback_url
      t.remove :js_context
      t.string :session_vars
    end
  end

  def down
    change_table :call_logs do |t|
      t.remove :session_vars
      t.string :callback_url
      t.binary :js_context
    end
  end
end
