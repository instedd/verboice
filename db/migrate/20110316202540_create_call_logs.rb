class CreateCallLogs < ActiveRecord::Migration
  def self.up
    create_table :call_logs do |t|
      t.integer :account_id
      t.integer :application_id
      t.datetime :finished_at
      t.string :direction
      t.string :address
      t.string :state
      t.text :details

      t.timestamps
    end
  end

  def self.down
    drop_table :call_logs
  end
end
