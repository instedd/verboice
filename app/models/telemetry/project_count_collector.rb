module Telemetry::ProjectCountCollector

  def self.collect_stats(period)
    {
      "counters" => [
        {
          "metric" => "projects",
          "key" => {},
          "value" => Project.count
        }
      ]
    }
  end

end
