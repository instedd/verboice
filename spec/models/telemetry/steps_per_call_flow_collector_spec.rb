require 'spec_helper'

describe Telemetry::StepsPerCallFlowCollector do

  it "builds counters for steps in each call flow" do
    p1 = Project.make languages: [:eng, :spa]
    p2 = Project.make languages: [:ger, :afr]

    f1 = p1.call_flows.make user_flow: [step, step, step]
    f2 = p1.call_flows.make user_flow: [step, step]
    f3 = p2.call_flows.make user_flow: [step]

    current_stats.should eq({
      "counters"=> [
        {
          "metric" => "steps",
          "key" => {"project_id" => p1.id, "call_flow"=>f1.id},
          "value" => 3
        },
        {
          "metric" => "steps",
          "key" => {"project_id" => p1.id, "call_flow"=>f2.id},
          "value" => 2
        },
        {
          "metric" => "steps",
          "key" => {"project_id" => p2.id, "call_flow"=>f3.id},
          "value" => 1
        }
      ]})
  end

  def current_stats
    period  = InsteddTelemetry.current_period
    Telemetry::StepsPerCallFlowCollector.collect_stats(p)
  end

  def step
    {
      'id' => 27,
      'root' => 2,
      'type' => 'play',
      'name' => 'Play number 27',
      'resource' => {
        "guid" => "resource 27 guid"
      }
    }
  end

end
