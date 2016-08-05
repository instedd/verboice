require 'spec_helper'

describe Telemetry::AccountsCollector do

  it "counts accounts for current period" do
    3.times { Account.make }
    period = InsteddTelemetry::Period.current

    stats = Telemetry::AccountsCollector.collect_stats(period)

    assert_equal stats, {
      "counters" => [
        {
        "metric" => "accounts",
        "key" => {},
        "value" => 3
        }
      ]
    }
  end

  it "takes into account period date" do
    Timecop.freeze(Time.now)
    3.times { Account.make }
    p0 = InsteddTelemetry::Period.current

    Timecop.freeze(Time.now + InsteddTelemetry::Period.span)
    2.times { Account.make }
    p1 = InsteddTelemetry::Period.current

    assert_equal Telemetry::AccountsCollector.collect_stats(p0), {
      "counters" => [{
        "metric" => "accounts",
        "key" => {},
        "value" => 3
      }]
    }

    assert_equal Telemetry::AccountsCollector.collect_stats(p1), {
      "counters" => [{
        "metric" => "accounts",
        "key" => {},
        "value" => 5
      }]
    }
  end

end
