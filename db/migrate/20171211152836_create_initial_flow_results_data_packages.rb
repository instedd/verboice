class CreateInitialFlowResultsDataPackages < ActiveRecord::Migration
  def up
    ActiveRecord::Base.connection.execute <<-SQL
      INSERT INTO flow_results_data_packages
        (flow_results_data_packages.uuid,
        flow_results_data_packages.call_flow_id,
        flow_results_data_packages.from,
        flow_results_data_packages.until,
        flow_results_data_packages.created_at,
        flow_results_data_packages.updated_at)
        SELECT UUID(), c.id, NOW(), NULL, NOW(), NOW()
        FROM call_flows c
    SQL
  end

  def down
  end
end
