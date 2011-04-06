class AddChannelIdToCallLog < ActiveRecord::Migration
  def self.up
    add_column :call_logs, :channel_id, :integer
  end

  def self.down
    remove_column :call_logs, :channel_id
  end
end
