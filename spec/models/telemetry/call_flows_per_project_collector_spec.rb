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

  it "projects with 0 call flows" do
    period = InsteddTelemetry::Period.current

    project_1 = Project.make created_at: period.end - 5.days
    project_2 = Project.make created_at: period.end - 1.day
    project_3 = Project.make created_at: period.end + 1.day

    CallFlow.make project: project_2, created_at: period.end + 1.day
    CallFlow.make project: project_3, created_at: period.end + 3.days

    counters = stats(period)['counters']

    counters.size.should eq(2)

    project_1_stat = counters.find{|x| x['key']['project_id'] == project_1.id}
    project_2_stat = counters.find{|x| x['key']['project_id'] == project_2.id}

    project_1_stat.should eq({
      "metric"  => "call_flows",
      "key"   => {"project_id" => project_1.id},
      "value" => 0
    })

    project_2_stat.should eq({
      "metric"  => "call_flows",
      "key"   => {"project_id" => project_2.id},
      "value" => 0
    })
  end

  def stats(period)
    Telemetry::CallFlowsPerProjectCollector.collect_stats(period)
  end

end
