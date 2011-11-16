class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  serialize :flow, Array

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    options = {:call_log => call_log, :address => address}
    if callback_url
      options[:application] = Application.new :callback_url => callback_url
    elsif flow
      options[:application] = Application.new :flow => flow
    end
    channel.new_session options
  end
end
