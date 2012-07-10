class AddTimeZoneToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :time_zone, :string
  end
end
