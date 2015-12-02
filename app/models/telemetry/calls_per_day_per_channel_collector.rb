module Telemetry::CallsPerDayPerChannelCollector

  def self.collect_stats(period)
    query = CallLog.select(['DATE(started_at)', 'channel_id', 'state', 'count(*)'])
                   .where('state != "active"')
                   .where('state != "queued"')
                   .where('started_at IS NOT NULL')
                   .where('channel_id IS NOT NULL')
                   .where('created_at >= ?', period.beginning)
                   .where('created_at < ?', period.end)
                   .group(['DATE(started_at)', 'channel_id', 'state'])
                   .to_sql

    results = ActiveRecord::Base.connection.execute query

    counters = results.map do |date, channel_id, state, count|
      {
        "metric" => "calls",
        "key" => { "channel_id" => channel_id, "date" => date.iso8601, "state" => state },
        "value" => count
      }
    end

    { "counters" => counters }
  end

end
