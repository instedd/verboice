class Time
  Numbers = %w(one two three four five six seven eight nine ten)

  def milliseconds
    ((to_f - to_f.floor) * 1000).floor
  end

  def self.smart_parse(time)
    if time.include?('ago') ||
      time.include?('year') || time.include?('month') || time.include?('day') ||
      time.include?('hour') || time.include?('minute') || time.include?('second')

      # Replace words with numbers
      Numbers.each_with_index do |n, i|
        time = (i + 1).to_s << time[n.length .. -1] if time.starts_with?(n)
      end

      return nil if time.to_i == 0

      time = time.gsub(' ', '.')
      result = eval(time)
      result.class <= Time || result.class <= ActiveSupport::TimeWithZone ? result : nil
    else
      parse(time)
    end
  rescue Exception => e
    nil
  end
end
