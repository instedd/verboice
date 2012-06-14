class MoveSchedulesToProjects < ActiveRecord::Migration
  def up
    add_column :schedules, :project_id, :integer
    add_index :schedules, :project_id

    connection.execute("INSERT INTO schedules(project_id, name, retries, time_from, time_to, weekdays) SELECT p.id, s.name, s.retries, s.time_from, s.time_to, s.weekdays FROM projects p, schedules s WHERE p.account_id = s.account_id")
    connection.execute("DELETE FROM schedules WHERE project_id IS NULL")

    remove_column :schedules, :account_id
  end

  def down
    add_column :schedules, :account_id, :integer
    add_index :schedules, :account_id

    connection.execute("INSERT INTO schedules(account_id, name, retries, time_from, time_to, weekdays) SELECT p.account_id, s.name, s.retries, s.time_from, s.time_to, s.weekdays FROM projects p, schedules s WHERE p.id = s.project_id LIMIT 1")
    connection.execute("DELETE FROM schedules WHERE account_id IS NULL")

    remove_column :schedules, :project_id
  end
end