class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  belongs_to :schedule
  belongs_to :project

  serialize :flow, Command

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    options = {:call_log => call_log, :address => address}
    if callback_url.present?
      options[:project] = Project.new :callback_url => callback_url
    elsif flow.present?
      options[:project] = Project.new :flow => flow
    end

    if status_callback_url.present? && options[:project]
      options[:project].status_callback_url = status_callback_url
    end

    channel.new_session options
  end
end
