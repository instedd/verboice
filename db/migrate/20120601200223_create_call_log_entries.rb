class CreateCallLogEntries < ActiveRecord::Migration
  def change
    create_table :call_log_entries do |t|
      t.references :call
      t.string :step_name
      t.string :step_id
      t.text :description
      t.string :severity

      t.timestamps
    end
    add_index :call_log_entries, :call_id
  end
end
