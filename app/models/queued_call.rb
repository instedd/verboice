class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
end
