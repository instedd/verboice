module Telemetry::ActiveChannelsCollector
  def self.collect_stats(period)
    period_end = ActiveRecord::Base.sanitize(period.end)

    results = ActiveRecord::Base.connection.execute <<-SQL
      SELECT channels.type, count(*)
      FROM channels
      WHERE channels.created_at < #{period_end}
      AND EXISTS (
        SELECT 1
        FROM call_logs
        WHERE call_logs.channel_id = channels.id
        AND call_logs.state = 'completed'
        LIMIT 1
      )
      GROUP BY channels.type
    SQL

    counters = results.map do |type, count|
      type = type.split('::').last.underscore
      {
        metric: 'active_channels',
        key: {type: type},
        value: count
      }
    end

    {counters: counters}
  end
end
