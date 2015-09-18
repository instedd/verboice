module Telemetry::CallsPerDayPerChannelCollector

  def self.collect_stats(period)
    results = ActiveRecord::Base.connection.execute <<-SQL
      SELECT DATE(started_at), channel_id, state, count(1)
      FROM call_logs
      WHERE state != "active" AND state != "queued"
      GROUP BY DATE(started_at), channel_id, state
    SQL

    counters = results.map do |date, channel_id, state, count|
      {
        "type" => "calls",
        "key" => { "channel_id" => channel_id, "date" => date.iso8601, "state" => state },
        "value" => count
      }
    end

    { "counters" => counters }
  end

end