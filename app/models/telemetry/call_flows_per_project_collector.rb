module Telemetry::CallFlowsPerProjectCollector

  def self.collect_stats(period)
    query = CallFlow.select(["project_id", "count(*)"])
                    .where("created_at < ?", period.end)
                    .group("project_id")
                    .to_sql

    results = ActiveRecord::Base.connection.execute query


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
