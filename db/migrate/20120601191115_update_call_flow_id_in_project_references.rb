class UpdateCallFlowIdInProjectReferences < ActiveRecord::Migration
  def up
    connection.execute("UPDATE channels t SET t.call_flow_id=(SELECT c.id FROM call_flows c WHERE c.project_id = t.call_flow_id LIMIT 1)")
    connection.execute("UPDATE traces t SET t.call_flow_id=(SELECT c.id FROM call_flows c WHERE c.project_id = t.call_flow_id LIMIT 1)")
    connection.execute("UPDATE queued_calls t SET t.call_flow_id=(SELECT c.id FROM call_flows c WHERE c.project_id = t.project_id LIMIT 1)")
    connection.execute("UPDATE call_logs t SET t.call_flow_id=(SELECT c.id FROM call_flows c WHERE c.project_id = t.project_id LIMIT 1)")
  end

  def down
  end
end
