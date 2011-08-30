class BaseBroker
  class << self
    attr_accessor :instance
  end

  def notify_call_queued channel
    queued_call = channel.poll_call
    call queued_call if queued_call
  end
end
