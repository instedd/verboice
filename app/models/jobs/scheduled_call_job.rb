class Jobs::ScheduledCallJob < Struct.new(:scheduled_call_id, :from, :to)

  # https://github.com/collectiveidea/delayed_job/pull/355
  Delayed::Backend::ActiveRecord::Job.send(:attr_accessible, :scheduled_call_id)

  def perform
    scheduled_call.make_calls self.from, self.to
  rescue ActiveRecord::RecordNotFound
    # ScheduledCall was deleted, pass
  end

  def success
    schedule_next_job
  end

  def failure
    schedule_next_job
  end

  def max_attempts
    1
  end

private

  def schedule_next_job
    scheduled_call.schedule_job
  rescue ActiveRecord::RecordNotFound
    # ScheduledCall was deleted, pass
  end

  def scheduled_call
    @scheduled_call ||= ScheduledCall.find(self.scheduled_call_id)
  end
end
