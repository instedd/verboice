module BaseAdapter
  def run
    new_session.run
  end

  def new_session
    channel = Channel.find channel_id
    call_log = CallLog.find call_log_id if call_log_id
    channel.new_session self, :call_log => call_log, :caller_id => caller_id
  end
end
