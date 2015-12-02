require 'spec_helper'

describe Telemetry::ProjectCountCollector do

  it "builds count of current projects in the application" do
    d0 = DateTime.new(2011,1,1,8,0,0)
    d1 = d0 + InsteddTelemetry::Period.span
    d2 = d1 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)
    10.times { Project.make }

    Timecop.freeze(d1)
    20.times { Project.make }
    period  = InsteddTelemetry.current_period

    Timecop.freeze(d2)
    Project.make

    stats(period).should eq({
      "counters" => [
        {
          "metric"  => "projects",
          "key"   => {},
          "value" => 30
        }
      ]
    })
  end

  def stats(period)
    Telemetry::ProjectCountCollector.collect_stats(period)
  end

end
