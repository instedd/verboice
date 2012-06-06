class RemoveStepIdAndNameFromCallFlowEntriesAndRenameDescriptionWithDetails < ActiveRecord::Migration
  def up
    connection.execute("SELECT id, step_name, step_id, description FROM call_log_entries").each do |id, step_name, step_id, description|
      text_description = YAML.dump({step_name: step_name, step_id: step_id, description: description})
      sanitized_assignment = ActiveRecord::Base.send(:sanitize_sql_for_assignment, description: text_description)

      connection.execute("UPDATE call_log_entries SET #{sanitized_assignment} WHERE id=#{id}")
    end

    rename_column :call_log_entries, :description, :details
    remove_column :call_log_entries, :step_name
    remove_column :call_log_entries, :step_id
  end

  def down
    rename_column :call_log_entries, :details, :description
    add_column :call_log_entries, :step_id, :string
    add_column :call_log_entries, :step_name, :string

    connection.execute("SELECT id, description FROM call_log_entries").each do |id, text_description|

      description = YAML.load(text_description)
      sanitized_assignment = ActiveRecord::Base.send(:sanitize_sql_for_assignment, description: description[:description], step_name: description[:step_name], step_id: description[:step_id])

      connection.execute("UPDATE call_log_entries SET #{sanitized_assignment} WHERE id=#{id}")
    end
  end
end
