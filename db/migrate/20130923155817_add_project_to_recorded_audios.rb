class AddProjectToRecordedAudios < ActiveRecord::Migration
  def up
    add_column :recorded_audios, :project_id, :integer

    execute <<-SQL
      UPDATE recorded_audios SET project_id = (SELECT project_id FROM contacts WHERE contact_id = contacts.id)
    SQL

    add_index :recorded_audios, [:project_id, :created_at]
  end

  def down
    remove_index :recorded_audios, [:project_id, :created_at]
    remove_column :recorded_audios, :project_id
  end
end
