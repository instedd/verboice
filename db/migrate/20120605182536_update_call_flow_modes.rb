class UpdateCallFlowModes < ActiveRecord::Migration
  def up
    connection.execute("UPDATE call_flows SET MODE = 'callback_url' WHERE callback_url <> ""'' ")
    connection.execute("UPDATE call_flows SET MODE = 'flow' WHERE callback_url = ''")
  end

  def down
  end
end
