class CreateScheduledCalls < ActiveRecord::Migration
  def change
    create_table :scheduled_calls do |t|
      t.string :name
      t.boolean :enabled, default: true
      t.references :project
      t.references :call_flow
      t.references :channel
      t.references :schedule
      t.string :frequency
      t.boolean :not_before_enabled, default: false
      t.datetime :not_before
      t.string :time_zone
      t.text :filters

      t.timestamps
    end
    add_index :scheduled_calls, :project_id
    add_index :scheduled_calls, :call_flow_id
    add_index :scheduled_calls, :channel_id
    add_index :scheduled_calls, :schedule_id
  end
end
