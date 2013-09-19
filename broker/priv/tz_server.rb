require "bundler/setup"
require "tzinfo"
require "active_support/time_with_zone"

loop do
  break unless tzname = STDIN.gets
  tzname.strip!
  begin
    tz = ActiveSupport::TimeZone.find_tzinfo(tzname)
    STDOUT.puts tz.current_period.utc_offset
    STDOUT.flush
  rescue Exception => ex
    STDOUT.puts ex.message
    STDOUT.flush
  end
end
