# This migration comes from instedd_telemetry (originally 20150828215634)
class CreateSettingsStore < ActiveRecord::Migration
  def change
    create_table :instedd_telemetry_settings do |t|
      t.string :key,   nullable: false
      t.string :value
    end

    add_index :instedd_telemetry_settings, :key, unique: true
  end
end
