class ChangeFromTimeAndToTimeInScheduledCall < ActiveRecord::Migration
  def up
    change_column :scheduled_calls, :from_time, :integer
    change_column :scheduled_calls, :to_time, :integer
  end

  def down
    change_column :scheduled_calls, :from_time, :time
    change_column :scheduled_calls, :to_time, :time
  end
end
