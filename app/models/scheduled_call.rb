class ScheduledCall < ActiveRecord::Base
  belongs_to :project
  belongs_to :call_flow
  belongs_to :channel

  attr_accessible :name, :enabled, :call_flow_id, :channel_id,
                  :not_before_enabled, :not_before_date, :not_before_time,
                  :not_after_enabled, :not_after_date, :not_after_time,
                  :time_zone, :filters, :from_time_hours, :to_time_hours, :recurrence_rule,
                  :filters_json

  validates :name, :project, :call_flow, :channel, :time_zone,
            :recurrence, :from_time, :to_time, presence: true

  serialize :filters, Array

  after_initialize :set_default_recurrence
  before_save :set_recurrence_start_time
  after_save :schedule_job
  before_update :delete_jobs
  after_destroy :delete_jobs

  has_recurrence :recurrence

  def matched_contacts
    ContactsFinder.for(self.project).find(self.filters)
  end

  def make_calls(from, to)
    return unless self.enabled

    call_options = {
      account: self.project.account,
      call_flow_id: self.call_flow_id,
      project_id: self.project_id,
      not_before: from,
      not_after: to
    }

    matched_contacts.each do |contact|
      self.channel.call contact.first_address, call_options
    end
  end

  def schedule_job
    return unless self.enabled

    run_at = run_time_at next_occurrence

    return if self.not_after_enabled && run_at > self.not_after

    start_time = start_time_at run_at
    end_time = end_time_at run_at

    Delayed::Job.enqueue Jobs::ScheduledCallJob.new(self.id, start_time, end_time),
      :scheduled_call_id => self.id,
      :run_at => run_at
  end

  def delete_jobs
    Delayed::Job.where(scheduled_call_id: self.id).delete_all
  end

  def next_occurrence
    now = Time.now
    from = self.not_before_enabled ? [now, self.not_before].max : now
    recurrence.next_occurrence(from)
  end

  def filters_json
    self.filters.to_json
  end

  def filters_json=(value)
    self.filters = JSON.parse(value)
  end

  def from_time_hours
    self.from_time.present? ? self.from_time.strftime('%H:%M') : nil
  end

  def from_time_hours=(value)
    self.from_time = value
  end

  def to_time_hours
    self.to_time.present? ? self.to_time.strftime('%H:%M') : nil
  end

  def to_time_hours=(value)
    self.to_time = value
  end

  def not_before_date
    self.not_before.present? ? self.not_before.strftime('%Y-%m-%d') : nil
  end

  def not_before_date=(value)
    return unless value.present?
    value = Time.parse(value)
    if self.not_before.present?
      self.not_before = self.not_before.change year: value.year, month: value.month, day: value.day
    else
      self.not_before = value
    end
  end

  def not_before_time
    self.not_before.present? ? self.not_before.strftime('%H:%M') : nil
  end

  def not_before_time=(value)
    return unless value.present?
    value = Time.parse(value)
    if self.not_before.present?
      self.not_before = self.not_before.change hour: value.hour, min: value.min
    else
      self.not_before = value
    end
  end

  def not_after_date
    self.not_after.present? ? self.not_after.strftime('%Y-%m-%d') : nil
  end

  def not_after_date=(value)
    return unless value.present?
    value = Time.parse(value)
    if self.not_after.present?
      self.not_after = self.not_after.change year: value.year, month: value.month, day: value.day
    else
      self.not_after = value
    end
  end

  def not_after_time
    self.not_after.present? ? self.not_after.strftime('%H:%M') : nil
  end

  def not_after_time=(value)
    return unless value.present?
    value = Time.parse(value)
    if self.not_after.present?
      self.not_after = self.not_after.change hour: value.hour, min: value.min
    else
      self.not_after = value
    end
  end

private

  def set_default_recurrence
    if new_record? && !self.recurrence
      self.recurrence = default_recurrence
    end
  end

  def default_recurrence
    r = IceCube::Schedule.new
    r.add_recurrence_rule IceCube::Rule.weekly.day(:monday)
    r
  end

  def set_recurrence_start_time
    self.recurrence_start_time = get_date_with_time self.recurrence_start_time, nil, tz
  end

  def run_time_at(date)
    get_date_with_time date, nil, tz
  end

  def start_time_at(date)
    get_date_with_time date, self.from_time, tz
  end

  def end_time_at(date)
    get_date_with_time date, self.to_time, tz
  end

  def tz
    ActiveSupport::TimeZone.new(self.time_zone)
  end

  def get_date_with_time(date, time, tz)
    year = date.year
    month = date.month
    day = date.day
    hour =  time.present? ? time.hour : 0
    min = time.present? ? time.min : 0
    offset = tz.formatted_offset
    Time.new(year, month, day, hour, min, 0, offset)
  end
end
