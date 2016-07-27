module Telemetry::AccountsCollector

  def self.collect_stats(period)
    {
      "counters" => [{
        "metric" => "accounts",
        "key"    => { },
        "value"  => Account.where("created_at < ?", period.end).count,
      }]
    }
  end

end
