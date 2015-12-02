# This migration comes from instedd_telemetry (originally 20150811184911)
class CreateCounters < ActiveRecord::Migration
  def change
    create_table :instedd_telemetry_counters do |t|
      t.integer :period_id
      t.string  :bucket,          nullable: :false
      t.text    :key_attributes,  nullable: :false
      t.integer :count,           nullable: :false, default: 0
    end
  end
end
