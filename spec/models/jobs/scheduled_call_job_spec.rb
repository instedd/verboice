require 'spec_helper'

describe Jobs::ScheduledCallJob do
  let(:scheduled_call) { ScheduledCall.make }
  let(:from) { Time.utc(2014, 12, 4, 10, 0, 0) }
  let(:to) { Time.utc(2014, 12, 4, 16, 0, 0) }

  let!(:job) { Jobs::ScheduledCallJob.new(scheduled_call.id, from, to) }

  before :each do
    ScheduledCall.stub(:find).with(scheduled_call.id).and_return(scheduled_call)
  end

  it 'should perform calls' do
    scheduled_call.should_receive(:make_calls).with(from, to)

    job.perform
  end

  it 'should perform once' do
    job.max_attempts.should eq(1)
  end

  [:success, :failure].each do |m|
    it "should scheduled next job when #{m}" do
      scheduled_call.should_receive(:schedule_job)

      job.send(m)
    end
  end
end
