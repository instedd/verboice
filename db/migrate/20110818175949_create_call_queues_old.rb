class CreateCallQueuesOld < ActiveRecord::Migration
  def self.up
    create_table :call_queues do |t|
      t.references :channel
      t.references :call_log
      t.string :address

      t.timestamps
    end
  end

  def self.down
    drop_table :call_queues
  end
end
