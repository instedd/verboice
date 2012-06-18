class RenameDataDirectoriesToCallFlows < ActiveRecord::Migration
  def up
    require 'fileutils'
    FileUtils.makedirs File.join(Rails.root, 'data', 'call_flows')

    connection.select_rows("SELECT id, project_id FROM call_flows").each do |call_flow_id, project_id|
      project_dir = File.join(Rails.root, 'data', 'projects', project_id.to_s)
      call_flow_dir = File.join(Rails.root, 'data', 'call_flows', call_flow_id.to_s)

      if File.directory?(project_dir) && !File.directory?(call_flow_dir)
        FileUtils.mv project_dir, call_flow_dir
      end
    end
  end

  def down
    require 'fileutils'
    FileUtils.makedirs File.join(Rails.root, 'data', 'projects')

    connection.select_rows("SELECT id, project_id FROM call_flows").each do |call_flow_id, project_id|
      project_dir = File.join(Rails.root, 'data', 'projects', project_id.to_s)
      call_flow_dir = File.join(Rails.root, 'data', 'call_flows', call_flow_id.to_s)

      if File.directory?(call_flow_dir) && !File.directory?(project_dir)
        FileUtils.mv call_flow_dir, project_dir
      end
    end
  end

end
