class CreateContactScheduledCalls < ActiveRecord::Migration
  def change
    create_table :contact_scheduled_calls do |t|
      t.references :contact
      t.references :scheduled_call
      t.datetime :last_called_at

      t.timestamps
    end
    add_index :contact_scheduled_calls, :contact_id
    add_index :contact_scheduled_calls, :scheduled_call_id
  end
end
