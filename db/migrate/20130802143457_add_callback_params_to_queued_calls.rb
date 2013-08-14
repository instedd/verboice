class AddCallbackParamsToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :callback_params, :text
  end
end
