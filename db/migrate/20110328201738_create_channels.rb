class CreateChannels < ActiveRecord::Migration
  def self.up
    create_table :channels do |t|
      t.integer :account_id
      t.integer :application_id
      t.string :name
      t.text :config

      t.timestamps
    end
  end

  def self.down
    drop_table :channels
  end
end
