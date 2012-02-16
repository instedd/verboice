class CallQueue < ActiveRecord::Base
  belongs_to :account
  has_many :queued_calls

  validates_presence_of :account
  validates_presence_of :name
  validates_uniqueness_of :name, :case_sensitive => false, :scoped_to => :account_id
  validates_format_of :retries, :with => /^[0-9\.]+(,[0-9\.]+)*$/, :allow_blank => true

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
    return t unless time_to && time_from
    from = time_from.as_seconds
    to = time_to.as_seconds
    time = t.as_seconds

    if time < from && (time > to || to > from)
      t + (from - time)
    elsif time > to && to > from
      t + (from - time) + 1.day
    else
      t
    end
  end

  private

  def time_str(time)
    return '' unless time
    "#{time.hour}:#{'%02d' % time.min}"
  end
end
