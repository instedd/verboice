class CreateTraces < ActiveRecord::Migration
  def change
    create_table :traces do |t|
      t.references :application
      t.integer :step_id
      t.integer :call_id
      t.string :result

      t.timestamps
    end
    add_index :traces, :application_id
  end
end
