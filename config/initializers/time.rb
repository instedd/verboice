# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

class Time
  Numbers = %w(one two three four five six seven eight nine ten)

  def milliseconds
    ((to_f - to_f.floor) * 1000).floor
  end

  def as_seconds
    self - self.at_beginning_of_day
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
