class ExtractCallFlowsFromProjects < ActiveRecord::Migration
  def up

    connection.execute("INSERT INTO call_flows(project_id, flow, user_flow, error_flow, callback_url, name) SELECT id, flow, user_flow, error_flow, callback_url, name FROM projects")

    remove_column :projects, :callback_url
    remove_column :projects, :flow
    remove_column :projects, :user_flow
    remove_column :projects, :error_flow
  end

  def down
    change_table :projects do |t|
      t.binary :flow
      t.binary :user_flow
      t.binary :error_flow
      t.string :callback_url
    end

    connection.execute("UPDATE projects SET flow=(SELECT flow FROM call_flows WHERE project_id = projects.id LIMIT 1), user_flow=(SELECT user_flow FROM call_flows WHERE project_id = projects.id LIMIT 1), error_flow=(SELECT error_flow FROM call_flows WHERE project_id = projects.id LIMIT 1), callback_url=(SELECT callback_url FROM call_flows WHERE project_id = projects.id LIMIT 1)")
  end
end
