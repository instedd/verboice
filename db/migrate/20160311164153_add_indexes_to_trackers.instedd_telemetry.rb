# This migration comes from instedd_telemetry (originally 20160309193418)
class AddIndexesToTrackers < ActiveRecord::Migration
  def change
    add_column :instedd_telemetry_counters, :key_attributes_hash, :string
    add_column :instedd_telemetry_set_occurrences, :key_attributes_hash, :string
    add_column :instedd_telemetry_timespans, :key_attributes_hash, :string

    remove_duplicates :instedd_telemetry_counters, [:bucket, :key_attributes, :period_id]
    remove_duplicates :instedd_telemetry_set_occurrences, [:bucket, :key_attributes, :element, :period_id]
    remove_duplicates :instedd_telemetry_timespans, [:bucket, :key_attributes]

    update_key_attributes_hash :instedd_telemetry_counters
    update_key_attributes_hash :instedd_telemetry_set_occurrences
    update_key_attributes_hash :instedd_telemetry_timespans

    add_index :instedd_telemetry_counters, [:bucket, :key_attributes_hash, :period_id], unique: true, name: "instedd_telemetry_counters_unique_fields"
    add_index :instedd_telemetry_set_occurrences, [:bucket, :key_attributes_hash, :element, :period_id], unique: true, name: "instedd_telemetry_set_occurrences_unique_fields"
    add_index :instedd_telemetry_timespans, [:bucket, :key_attributes_hash], unique: true, name: "instedd_telemetry_timespans_unique_fields"
  end

  def remove_duplicates(table_name, unique_fields)
    ActiveRecord::Base.connection.execute <<-SQL
      DELETE
      FROM #{table_name}
      WHERE id NOT IN (
        SELECT MIN(t.id)
        FROM (SELECT #{([:id] + unique_fields).join(', ')} FROM #{table_name}) AS t
        GROUP BY #{unique_fields.map{|x| "t.#{x}"}.join(', ')}
      )
    SQL
  end

  def update_key_attributes_hash(table_name)
    records = ActiveRecord::Base.connection.execute <<-SQL
      SELECT id, key_attributes
      FROM #{table_name}
    SQL

    records.each do |id, key_attributes|
      hash = Digest::SHA256.hexdigest key_attributes
      ActiveRecord::Base.connection.execute "UPDATE #{table_name} SET key_attributes_hash = '#{hash}' WHERE id = #{id}"
    end
  end
end
