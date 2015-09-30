class CreateImpersonateRecords < ActiveRecord::Migration
  def change
    create_table :impersonate_records do |t|
      t.references :call_flow
      t.references :contact
      t.integer :impersonated_id

      t.timestamps
    end
    add_index :impersonate_records, :call_flow_id
    add_index :impersonate_records, :contact_id
  end
end
