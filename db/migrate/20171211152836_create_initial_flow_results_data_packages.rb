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
        SELECT UUID(), c.id, c.updated_at, NULL, NOW(), NOW()
        FROM call_flows c
        WHERE c.mode = 'flow'
        AND NOT EXISTS (SELECT 1 FROM flow_results_data_packages f WHERE f.call_flow_id = c.id AND f.until IS NULL)
    SQL
  end

  def down
  end
end
