class AddStatusCallbackUrlToQueuedCall < ActiveRecord::Migration
  def change
    add_column :queued_calls, :status_callback_url, :string
  end
end
