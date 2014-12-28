class ChangeScheduleInScheduledCall < ActiveRecord::Migration
  def up
    remove_column :scheduled_calls, :schedule_id
    remove_column :scheduled_calls, :frequency
    add_column :scheduled_calls, :not_after_enabled, :boolean, default: false
    add_column :scheduled_calls, :not_after, :datetime
    add_column :scheduled_calls, :from_time, :time
    add_column :scheduled_calls, :to_time, :time
    add_column :scheduled_calls, :recurrence, :text
  end

  def down
    add_column :scheduled_calls, :schedule_id, :integer
    add_column :scheduled_calls, :frequency, :text
    remove_column :scheduled_calls, :not_after_enabled
    remove_column :scheduled_calls, :not_after
    remove_column :scheduled_calls, :from_time
    remove_column :scheduled_calls, :to_time
    remove_column :scheduled_calls, :recurrence
  end
end
