class ScheduledCall < ActiveRecord::Base
  belongs_to :project
  belongs_to :call_flow
  belongs_to :channel
  belongs_to :schedule

  attr_accessible :enabled, :filters, :frequency, :name, :not_before, :not_before_enabled, :time_zone

  validates :name, :project, :call_flow, :channel, :schedule, :frequency, :time_zone, presence: true

  serialize :filters, Array
end
