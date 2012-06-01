class AddModeToCallFlow < ActiveRecord::Migration
  def up
    add_column :call_flows, :mode, :string

    connection.execute("UPDATE CALL_FLOWS SET MODE = 'callback_url' WHERE CALLBACK_URL IS NOT NULL")
    connection.execute("UPDATE CALL_FLOWS SET MODE = 'flow' WHERE CALLBACK_URL IS NULL")
  end

  def down
    remove_column :call_flows, :mode
  end
end
