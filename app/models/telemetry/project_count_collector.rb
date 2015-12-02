module Telemetry::ProjectCountCollector

  def self.collect_stats(period)
    {
      "counters" => [
        {
          "metric" => "projects",
          "key" => {},
          "value" => Project.where("created_at < ?", period.end).count
        }
      ]
    }
  end

end
