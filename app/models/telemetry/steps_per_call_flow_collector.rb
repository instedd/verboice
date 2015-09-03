module Telemetry::StepsPerCallFlowCollector

  def self.collect_stats(period)
    counters = []

    CallFlow.select([:id, :user_flow]).find_each do |flow|
      counters << {
        "type" => "steps",
        "key" => { "call_flow" => flow.id },
        "value" => flow.user_flow.count
      }
    end

    { "counters" => counters }
  end

end