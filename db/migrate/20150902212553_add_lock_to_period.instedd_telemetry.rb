# This migration comes from instedd_telemetry (originally 20150828135438)
class AddLockToPeriod < ActiveRecord::Migration
  def change
    add_column :instedd_telemetry_periods, :lock_owner,      :string
    add_column :instedd_telemetry_periods, :lock_expiration, :datetime
  end
end
