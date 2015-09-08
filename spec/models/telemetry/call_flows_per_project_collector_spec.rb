require 'spec_helper'

describe Telemetry::CallFlowsPerProjectCollector do
  
  it "builds counts of current call flows per project" do
    p1 = Project.make languages: [:eng, :spa]
    p2 = Project.make languages: [:ger, :afr]

    10.times { p1.call_flows.make }
    30.times { p2.call_flows.make }

    current_stats.should eq({
      "counters" => [
        {
          "type"  => "call_flows",
          "key"   => {"project_id" => p1.id},
          "value" => 10
        },
        {
          "type"  => "call_flows",
          "key"   => {"project_id" => p2.id},
          "value" => 30
        }
      ]
    })
  end

  def current_stats
    period  = InsteddTelemetry.current_period
    Telemetry::CallFlowsPerProjectCollector.collect_stats(p)
  end

end
