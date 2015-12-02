module Telemetry::StepsPerCallFlowCollector

  def self.collect_stats(period)
    counters = []

    flows = CallFlow.select([:id, :project_id, :user_flow])
                    .where('created_at < ?', period.end)
    
    flows.find_each do |flow|
      step_count = flow.user_flow.present? ? flow.user_flow.count : 0
      counters << {
        "metric" => "steps",
        "key" => { "project_id" => flow.project_id, "call_flow" => flow.id },
        "value" => step_count
      }
    end

    { "counters" => counters }
  end

end
