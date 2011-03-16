class CallLog < ActiveRecord::Base
  belongs_to :account
  belongs_to :application

  def finish(state)
    self.state = state
    self.finished_at = Time.now.utc
    self.save!
  end
end
