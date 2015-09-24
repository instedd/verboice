require 'spec_helper'

describe Telemetry::ProjectCountCollector do

  it "builds count of current projects in the application" do
    40.times { Project.make }

    current_stats.should eq({
      "counters" => [
        {
          "metric"  => "projects",
          "key"   => {},
          "value" => 40
        }
      ]
    })
  end

  def current_stats
    period  = InsteddTelemetry.current_period
    Telemetry::ProjectCountCollector.collect_stats(p)
  end

end
