class AddLastActivityAtToContacts < ActiveRecord::Migration
  def up
    add_column :contacts, :last_activity_at, :datetime

    ActiveRecord::Base.connection.execute <<-SQL
      UPDATE contacts SET last_activity_at = (
        SELECT call_logs.finished_at
        FROM call_logs
        WHERE call_logs.finished_at IS NOT NULL
          AND call_logs.contact_id = contacts.id
        ORDER BY call_logs.finished_at DESC
        LIMIT 1
      )
    SQL
  end

  def down
    remove_column :contacts, :last_activity_at
  end
end
