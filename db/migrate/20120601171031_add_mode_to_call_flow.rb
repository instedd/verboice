class AddModeToCallFlow < ActiveRecord::Migration
  def up
    add_column :call_flows, :mode, :string

    connection.execute("UPDATE call_flows SET MODE = 'callback_url' WHERE callback_url IS NOT NULL")
    connection.execute("UPDATE call_flows SET MODE = 'flow' WHERE callback_url IS NULL")
  end

  def down
    remove_column :call_flows, :mode
  end
end
