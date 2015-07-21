class ScheduledCall < ActiveRecord::Base
  belongs_to :project
  belongs_to :call_flow
  belongs_to :channel

  has_many :contact_scheduled_calls, :dependent => :destroy

  attr_accessible :name, :enabled, :call_flow_id, :channel_id,
                  :not_before_enabled, :not_before_date, :not_before_time,
                  :not_after_enabled, :not_after_date, :not_after_time,
                  :time_zone, :filters, :from_time, :to_time, :recurrence_rule,
                  :filters_json

  validates :name, :project, :call_flow, :channel, :time_zone,
            :recurrence, :from_time, :to_time, presence: true

  validates :from_time, :to_time, numericality: {only_integer: true, greater_than_or_equal_to: 0, less_than_or_equal_to: 2880}

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
      not_after: to,
      scheduled_call_id: self.id
    }

    # get matched contacts and sort them by last called
    contacts = matched_contacts
    contacts_ids = contacts.map(&:id)
    contact_scheduled_calls = self.contact_scheduled_calls.where(contact_id: contacts_ids)
    contacts_last_called_at = contact_scheduled_calls.inject({}) do |h, x|
      h[x.contact_id] = x.last_called_at
      h
    end
    contacts.sort! do |a,b|
      a_last_called = contacts_last_called_at[a.id]
      b_last_called = contacts_last_called_at[b.id]
      if a_last_called.nil?
        -1
      elsif b_last_called.nil?
        1
      else
        a_last_called <=> b_last_called
      end
    end

    contacts.each do |contact|
      self.channel.call contact.first_address, call_options.merge(contact_id: contact.id)
    end
  end

  def schedule_job
    return unless self.enabled

    run_at = next_run_at

    return if self.not_after_enabled && run_at > not_after_with_tz

    start_time = start_time_at run_at
    end_time = end_time_at run_at

    Delayed::Job.enqueue Jobs::ScheduledCallJob.new(self.id, start_time, end_time),
      :scheduled_call_id => self.id,
      :run_at => run_at
  end

  def delete_jobs
    Delayed::Job.where(scheduled_call_id: self.id).delete_all
  end

  # Return the time at which to schedule the next job to enqueue the calls.
  # This should be at 00:00 of the next occurrence according to the schedule,
  # and the returned time must be in the scheduled call timezone.
  def next_run_at
    now = Time.now
    from = self.not_before_enabled ? [now, not_before_with_tz].max : now
    result = recurrence.next_occurrence(from).in_time_zone(tz)

    # This resets the time to 00:00, preserving day and time zone
    result = get_date_with_time result, nil, tz
    if result <= from
      # Skip a day if resulting time would be before the calculated from and try again
      result = recurrence.next_occurrence(from + 1.day).in_time_zone(tz)
      get_date_with_time result, nil, tz
    else
      result
    end
  end

  def filters_json
    self.filters.to_json
  end

  def filters_json=(value)
    self.filters = JSON.parse(value)
  end

  # split handling of not_before/not_after in date and time
  [:not_before, :not_after].each do |attr|
    define_method "#{attr}_date" do
      self.send(attr).try(:strftime, '%Y-%m-%d')
    end

    define_method "#{attr}_date=" do |value|
      return unless value.present?
      value = Time.parse(value)
      if self.send(attr).present?
        new_value = self.send(attr).change year: value.year, month: value.month, day: value.day
        self.send("#{attr}=", new_value)
      else
        self.send("#{attr}=", value)
      end
    end

    define_method "#{attr}_time" do
      self.send(attr).try(:strftime, '%H:%M')
    end

    define_method "#{attr}_time=" do |value|
      return unless value.present?
      value = Time.parse(value)
      if self.send(attr).present?
        new_value = self.send(attr).change hour: value.hour, min: value.min
        self.send("#{attr}=", new_value)
      else
        self.send("#{attr}=", value)
      end
    end
  end

  def description
    if from_time.present? && to_time.present?
      "#{self.recurrence.to_s} from #{format_time(from_time)} to #{format_time(to_time)}"
    else
      self.recurrence.to_s
    end
  end

private

  def set_default_recurrence
    if new_record? && !self.read_attribute(:recurrence)
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

  def start_time_at(date)
    get_date_with_time date, self.from_time, tz
  end

  def end_time_at(date)
    get_date_with_time date, self.to_time, tz
  end

  # not_before is saved in the table in UTC; this method returns it in the
  # scheduled call time zone
  def not_before_with_tz
    return nil unless self.not_before_enabled
    time = self.not_before.hour * 60 + self.not_before.min
    get_date_with_time self.not_before, time, tz
  end

  # not_after is saved in the table in UTC; this method returns it in the
  # scheduled call time zone
  def not_after_with_tz
    return nil unless self.not_after_enabled
    time = self.not_after.hour * 60 + self.not_after.min
    get_date_with_time self.not_after, time, tz
  end

  def tz
    ActiveSupport::TimeZone.new(self.time_zone)
  end

  def get_date_with_time(date, time, tz)
    year = date.year
    month = date.month
    day = date.day
    hour =  time.present? ? (time / 60 % 24) : 0
    min = time.present? ? (time % 60) : 0
    offset = tz.formatted_offset
    result = Time.new(year, month, day, hour, min, 0, offset).in_time_zone(tz)
    time.present? ? result + (time / (24 * 60)).day : result
  end

  def format_time(time_in_mins)
    h = (time_in_mins / 60 % 24).to_s
    m = (time_in_mins % 60).to_s
    h = h.length < 2 ? "0" + h : h;
    m = m.length < 2 ? "0" + m : m;
    s = "#{h}:#{m}"
    time_in_mins > 1440 ? "#{s} on next day" : s
  end
end
