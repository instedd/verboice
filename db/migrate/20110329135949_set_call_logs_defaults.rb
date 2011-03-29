class SetCallLogsDefaults < ActiveRecord::Migration
  def self.up
    change_column_default(:call_logs, :state, :active)
    change_column_default(:call_logs, :details, '')
  end

  def self.down
    change_column_default(:call_logs, :state, nil)
    change_column_default(:call_logs, :details, nil)
  end
end
