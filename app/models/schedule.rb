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

class Schedule < ActiveRecord::Base
  belongs_to :project
  has_many :queued_calls
  has_many :call_logs, :dependent => :nullify
  has_one :account, :through => :project

  validates_presence_of :account
  validates_presence_of :name
  validates_presence_of :time_from
  validates_presence_of :time_to
  validates_uniqueness_of :name, :case_sensitive => false, :scope => :project_id
  validates_format_of :retries, :with => /^[0-9\.]+(,[0-9\.]+)*$/, :allow_blank => true
  validates_format_of :weekdays, :with => /^[0-6](,[0-6])*$/, :allow_blank => true

  validate :time_from_is_before_time_to

  before_destroy :cancel_calls_and_destroy_queued_calls

  broker_cached

  def time_from_is_before_time_to
    if time_from && time_to
      unless time_from < time_to
        errors[:base] << "End time must be greater than the start time."
      end
    end
  end

  def time_zone=(value)
    @time_zone = value
  end

  def with_time_zone(tz)
    old_time_zone = @time_zone
    @time_zone = tz.kind_of?(String) ? ActiveSupport::TimeZone.new(tz || 'UTC') : tz
    value = yield self
    @time_zone = old_time_zone
    value
  end

  def time_from_str
    time_str(time_from)
  end

  def time_from_str=(value)
    self.time_from = value
  end

  def time_to_str
    time_str(time_to)
  end

  def time_to_str=(value)
    self.time_to = value
  end

  def next_available_time(t)
    t = Time.now.utc if t.past?

    if time_from.present? && time_to.present?
      from = get_seconds(time_from)
      to = get_seconds(time_to)
      time = t.as_seconds

      if time < from && (time > to || to > from)
        t = t + (from - time)
      elsif time > to && to > from
        t = t + (from - time) + 1.day
      end

    end

    if weekdays.present?
      available_days = Set.new(weekdays.split(',')).to_a.map(&:to_i).sort
      day = available_days.detect{|d| d >= t.wday} || available_days.first
      t = t + (day - t.wday + (day < t.wday ? 7 : 0)).days
    end

    t
  end

  def self.from_json(json)
    schedule = self.new
    schedule.name = json[:name]
    schedule.retries = json[:retries]
    schedule.time_from_str = json[:time_from_str]
    schedule.time_to_str = json[:time_to_str]
    schedule.weekdays = json[:weekdays]
    schedule.project_id = json[:project_id]
    schedule
  end

  def as_json(options={})
    super(options.merge({:only => [:name, :retries, :weekdays], :methods => [:time_from_str, :time_to_str]}))
  end

  def retry_delays
    retries.split(',').map &:to_f
  end

  def includes_day?(day)
    (weekdays || '').split(',').map(&:to_i).include?(day)
  end

  def description
    days_desc = (weekdays || '').split(',').map{|d| Date::DAYNAMES[d.to_i]}
    desc = "#{days_desc.to_sentence} from #{time_from_str} to #{time_to_str}."
    if retries.present?
      retries_desc = (retries || '').split(',').map{|r| "#{r} #{'hour'.pluralize(r.to_f)}"}
      desc << " Retry after #{retries_desc.to_sentence}."
    end
    desc
  end

  def cancel_calls_and_destroy_queued_calls
    self.queued_calls.each do |queued_call|
      queued_call.cancel_call!
    end
    self.queued_calls.destroy_all
  end

  private

  def get_seconds(time)
    time = move_to_time_zone(time, @time_zone) if @time_zone
    time.as_seconds
  end

  # Interprets a time as if it were in a different time zone
  def move_to_time_zone(d, tz)
    tz = ActiveSupport::TimeZone.new(tz) if tz.kind_of?(String)
    offset = tz.utc_offset / 60 / 60 / 24.0
    DateTime.civil(d.year, d.month, d.day, d.hour, d.min, d.sec, offset).utc.to_time
  end

  def time_str(time)
    return '' unless time
    "#{time.hour}:#{'%02d' % time.min}"
  end

end
