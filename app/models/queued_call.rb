class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    channel.new_session :call_log => call_log, :address => address
  end
end
