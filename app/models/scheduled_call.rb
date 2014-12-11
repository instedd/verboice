class ScheduledCall < ActiveRecord::Base
  belongs_to :project
  belongs_to :call_flow
  belongs_to :channel
  belongs_to :schedule

  attr_accessible :enabled, :filters, :frequency, :name, :not_before, :not_before_enabled, :time_zone

  validates :name, :project, :call_flow, :channel, :schedule, :frequency, :time_zone, presence: true

  serialize :filters, Array

  def matched_contacts
    ContactsFinder.for(self.project).find(self.filters)
  end

  def make_calls
    return unless self.enabled

    call_options = {
      account: self.project.account,
      schedule_id: self.schedule_id,
      time_zone: self.time_zone,
      call_flow_id: self.call_flow_id,
      project_id: self.project_id
    }

    if self.not_before_enabled
      tz = ActiveSupport::TimeZone.new(self.time_zone)
      call_options[:not_before] = self.not_before.in_time_zone tz
    end

    matched_contacts.each do |contact|
      self.channel.call contact.first_address, call_options
    end
  end
end
