class AddContactIdToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :contact_id, :integer
    add_index :call_logs, :contact_id
  end
end
