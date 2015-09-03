module Telemetry::ProjectCountCollector

  def self.collect_stats(period)
    {
      "counters" => [
        {
          "type" => "projects",
          "key" => {},
          "value" => Project.count
        }
      ]
    }
  end

end