class CreateNuntiumChannels < ActiveRecord::Migration
  def change
    create_table :nuntium_channels do |t|
      t.references :account
      t.string :name
      t.string :channel_name
      t.boolean :enabled

      t.timestamps
    end

    add_index :nuntium_channels, :account_id
  end
end
