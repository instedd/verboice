class AddScheduledCallIdToDelayedJob < ActiveRecord::Migration
  def change
    add_column :delayed_jobs, :scheduled_call_id, :integer
  end
end
