class CreateCallQueues < ActiveRecord::Migration
  def change
    create_table :call_queues do |t|
      t.integer :account_id
      t.string :name
      t.string :retries
      t.time :time_from
      t.time :time_to
      t.string :weekdays

      t.timestamps
    end
  end
end
