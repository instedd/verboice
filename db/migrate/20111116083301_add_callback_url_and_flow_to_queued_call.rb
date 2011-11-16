class AddCallbackUrlAndFlowToQueuedCall < ActiveRecord::Migration
  def self.up
    add_column :queued_calls, :callback_url, :string
    add_column :queued_calls, :flow, :text
  end

  def self.down
    remove_column :queued_calls, :flow
    remove_column :queued_calls, :callback_url
  end
end
