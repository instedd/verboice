require 'spec_helper'

describe Telemetry::StepsPerCallFlowCollector do

  it "builds counters for steps in each call flow" do
    d0 = DateTime.new(2011,1,1,8,0,0)
    d1 = d0 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)
    p1 = Project.make
    p2 = Project.make

    f1 = p1.call_flows.make user_flow: [step, step, step]
    f2 = p1.call_flows.make user_flow: [step, step]
    f3 = p2.call_flows.make user_flow: [step]

    period = InsteddTelemetry::Period.current

    Timecop.freeze(d1)
    p2.call_flows.make user_flow: [step]

    stats(period).should eq({
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

  it "supports empty call_flows" do
    p = Project.make
    f = p.call_flows.make user_flow: nil
    period = InsteddTelemetry::Period.current

    stats(period).should eq({
      "counters"=> [
        {
          "metric" => "steps",
          "key" => {"project_id" => p.id, "call_flow"=>f.id},
          "value" => 0
        }
    ]})
  end

  def stats(period)
    Telemetry::StepsPerCallFlowCollector.collect_stats(period)
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
