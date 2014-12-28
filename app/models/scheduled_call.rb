class ScheduledCall < ActiveRecord::Base
  belongs_to :project
  belongs_to :call_flow
  belongs_to :channel

  attr_accessible :name, :enabled, :call_flow_id, :channel_id,
                  :not_before, :not_before_enabled, :not_after, :not_after_enabled,
                  :time_zone, :filters, :from_time, :to_time, :recurrence_rule

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
