class CreateCallFlows < ActiveRecord::Migration
  def change
    create_table :call_flows do |t|
      t.string :name
      t.binary :flow
      t.binary :user_flow
      t.binary :error_flow
      t.string :callback_url
      t.integer :project_id
      t.text :encrypted_config

      t.timestamps
    end
    add_index :call_flows, :project_id
  end
end
