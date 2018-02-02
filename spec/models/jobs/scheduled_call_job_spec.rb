require 'spec_helper'

describe Jobs::ScheduledCallJob do
  let(:scheduled_call) { ScheduledCall.make }
  let(:from) { Time.utc(2014, 12, 4, 10, 0, 0) }
  let(:to) { Time.utc(2014, 12, 4, 16, 0, 0) }

  let!(:job) { Jobs::ScheduledCallJob.new(scheduled_call.id, from, to) }

  before :each do
    allow(ScheduledCall).to receive(:find).with(scheduled_call.id).and_return(scheduled_call)
  end

  it 'should perform calls' do
    expect(scheduled_call).to receive(:make_calls).with(from, to)

    job.perform
  end

  it 'should perform once' do
    expect(job.max_attempts).to eq(1)
  end

  [:success, :failure].each do |m|
    it "should scheduled next job when #{m}" do
      expect(scheduled_call).to receive(:schedule_job)

      job.send(m)
    end
  end
end
