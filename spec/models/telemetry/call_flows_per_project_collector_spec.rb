require 'spec_helper'

describe Telemetry::CallFlowsPerProjectCollector do

  it "builds counts of current call flows per project" do    
    d0 = DateTime.new(2011,1,1,8,0,0)
    d1 = d0 + InsteddTelemetry::Period.span
    d2 = d1 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)
    p1 = Project.make
    p2 = Project.make
    5.times { p1.call_flows.make }

    Timecop.travel(d1)
    5.times  { p1.call_flows.make }
    30.times { p2.call_flows.make }

    period  = InsteddTelemetry.current_period

    Timecop.travel(d2)
    p1.call_flows.make
    p2.call_flows.make

    stats(period).should eq({
      "counters" => [
        {
          "metric"  => "call_flows",
          "key"   => {"project_id" => p1.id},
          "value" => 10
        },
        {
          "metric"  => "call_flows",
          "key"   => {"project_id" => p2.id},
          "value" => 30
        }
      ]
    })
  end

  def stats(period)
    Telemetry::CallFlowsPerProjectCollector.collect_stats(period)
  end

end
