class AddStartedToCallLog < ActiveRecord::Migration
  def self.up
    add_column :call_logs, :started_at, :datetime
  end

  def self.down
    remove_column :call_logs, :started_at
  end
end