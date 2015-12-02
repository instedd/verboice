module Telemetry::CallFlowsPerProjectCollector

  def self.collect_stats(period)
    period_end = ActiveRecord::Base.sanitize(period.end)

    results = ActiveRecord::Base.connection.execute <<-SQL
      SELECT projects.id, COUNT(call_flows.project_id)
      FROM projects
      LEFT JOIN call_flows ON call_flows.project_id = projects.id
      AND call_flows.created_at < #{period_end}
      WHERE projects.created_at < #{period_end}
      GROUP BY projects.id
    SQL


    counters = results.map do |project_id, count|
      {
        "metric" => "call_flows",
        "key" => { "project_id" => project_id },
        "value" => count
      }
    end

    { "counters" => counters }
  end

end
