# This migration comes from instedd_telemetry (originally 20150810180903)
class CreateSets < ActiveRecord::Migration
  def change
    create_table :instedd_telemetry_set_occurrences do |t|
      t.integer :period_id
      t.string  :bucket,          nullable: :false
      t.text    :key_attributes,  nullable: :false
      t.string  :element,         nullable: :false
    end
  end
end
