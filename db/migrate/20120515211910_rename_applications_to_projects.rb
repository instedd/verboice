class RenameApplicationsToProjects < ActiveRecord::Migration
  def change
      rename_table :applications, :projects
      rename_column :call_logs, :application_id, :project_id
      rename_column :channels, :application_id, :project_id
      rename_column :queued_calls, :application_id, :project_id
      rename_column :traces, :application_id, :project_id
  end
end
