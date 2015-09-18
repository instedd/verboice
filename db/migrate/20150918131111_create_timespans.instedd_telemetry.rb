# This migration comes from instedd_telemetry (originally 20150909174056)
class CreateTimespans < ActiveRecord::Migration
  def change
    create_table :instedd_telemetry_timespans do |t|
      t.integer  :period_id
      t.string   :bucket,          nullable: :false
      t.text     :key_attributes,  nullable: :false
      t.datetime :since,           nullable: :false
      t.datetime :until,           nullable: :false
    end
  end
end
