module Telemetry::ActiveChannelsCollector
  extend InsteddTelemetry::StatCollectors::Utils

  def self.collect_stats(period)
    period_end = ActiveRecord::Base.sanitize(period.end)

    result = ActiveRecord::Base.connection.execute <<-SQL
      SELECT count(*)
      FROM channels
      WHERE channels.created_at < #{period_end}
      AND EXISTS (
        SELECT 1
        FROM call_logs
        WHERE call_logs.channel_id = channels.id
        AND call_logs.state = 'completed'
        LIMIT 1
      )
    SQL

    count = result.first.first

    simple_counter('active_channels', {}, count)
  end
end
