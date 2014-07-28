class CreateAlerts < ActiveRecord::Migration
  def change
    create_table :alerts do |t|
      t.references :account
      t.string :type
      t.string :severity
      t.string :message
      t.string :key
      t.text :data
      t.timestamps
    end
    add_index :alerts, [:account_id, :key]
  end
end
