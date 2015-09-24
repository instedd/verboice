module Telemetry::CallFlowsPerProjectCollector

  def self.collect_stats(period)
    results = ActiveRecord::Base.connection.execute <<-SQL
      SELECT project_id, count(1)
      FROM call_flows
      GROUP BY project_id
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
