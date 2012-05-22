class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  belongs_to :schedule
  belongs_to :project
  belongs_to :call_flow

  serialize :flow, Command

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    options = {:call_log => call_log, :address => address}

    options[:call_flow] = if call_flow.present?
      call_flow
    else
      CallFlow.new
    end

    if callback_url.present?
      options[:call_flow].callback_url = callback_url
    elsif flow.present?
      options[:call_flow].flow = flow
    end

    if project.present?
      options[:call_flow].project = project
    else
      options[:call_flow].project = Project.new
    end

    if status_callback_url.present? && options[:call_flow]
      options[:call_flow].project.status_callback_url = status_callback_url
    end

    channel.new_session options
  end
end
