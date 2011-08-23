class CallQueue < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  
  def self.enqueue channel, call_log, address
    CallQueue.create! :channel => channel, :call_log => call_log, :address => address
  end
  
  def self.poll channel
    CallQueue.transaction do
      call_queue = CallQueue.where('channel_id = ?', channel.id).order('created_at').first
      call_queue.destroy if call_queue
    end
  end
  
end
