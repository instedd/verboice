module Telemetry::StepsPerCallFlowCollector

  def self.collect_stats(period)
    counters = []

    CallFlow.select([:id, :project_id, :user_flow]).find_each do |flow|
      counters << {
        "metric" => "steps",
        "key" => { "project_id" => flow.project_id, "call_flow" => flow.id },
        "value" => flow.user_flow.count
      }
    end

    { "counters" => counters }
  end

end
