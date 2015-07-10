require 'spec_helper'

describe ScheduledCall do
  it { should belong_to(:project) }
  it { should belong_to(:call_flow) }
  it { should belong_to(:channel) }

  it { should validate_presence_of(:name) }
  it { should validate_presence_of(:project) }
  it { should validate_presence_of(:call_flow) }
  it { should validate_presence_of(:channel) }
  it { should validate_presence_of(:time_zone) }
  it { should validate_presence_of(:from_time) }
  it { should validate_presence_of(:to_time) }

  let!(:scheduled_call) { ScheduledCall.make from_time: 10 * 60, to_time: 15 * 60 }

  it "should find matched contacts" do
    scheduled_call.filters = {foo: 'bar'}
    finder = double('finder')
    ContactsFinder.should_receive(:for).with(scheduled_call.project).and_return(finder)
    finder.should_receive(:find).with(scheduled_call.filters).and_return([1,2,3])

    scheduled_call.matched_contacts.should eq([1,2,3])
  end

  it 'should delete related jobs' do
    Delayed::Job.where(scheduled_call_id: scheduled_call.id).count.should eq(1)
    scheduled_call.delete_jobs
    Delayed::Job.where(scheduled_call_id: scheduled_call.id).count.should eq(0)
  end

  describe 'calls' do
    before :each do
      @contact_a = Contact.make project: scheduled_call.project
      @contact_b = Contact.make project: scheduled_call.project

      scheduled_call.stub(:matched_contacts).and_return([@contact_a, @contact_b])
    end

    let(:from) { Time.new(2014,12,04,10) }
    let(:to) { Time.new(2014,12,04,15) }

    let(:expected_options) do
      {
        account: scheduled_call.project.account,
        call_flow_id: scheduled_call.call_flow_id,
        project_id: scheduled_call.project_id,
        not_before: from,
        not_after: to,
        scheduled_call_id: scheduled_call.id
      }
    end

    it "should make calls" do
      scheduled_call.channel.should_receive(:call).with(@contact_a.first_address, expected_options.merge(contact_id: @contact_a.id))
      scheduled_call.channel.should_receive(:call).with(@contact_b.first_address, expected_options.merge(contact_id: @contact_b.id))

      scheduled_call.make_calls from, to
    end

    it "should not make calls if disabled" do
      scheduled_call.enabled = false

      scheduled_call.channel.should_not_receive(:call)

      scheduled_call.make_calls from, to
    end

    describe 'order' do
      before :each do
        @contact_c = Contact.make project: scheduled_call.project

        scheduled_call.stub(:matched_contacts).and_return([@contact_a, @contact_b, @contact_c])
      end

      it 'should call contacts by last called order' do
        a_last_called = Time.utc(2014, 12, 04)
        b_last_called = a_last_called - 7.day
        c_last_called = a_last_called - 3.day

        ContactScheduledCall.make contact: @contact_a, scheduled_call: scheduled_call, last_called_at: a_last_called
        ContactScheduledCall.make contact: @contact_b, scheduled_call: scheduled_call, last_called_at: b_last_called
        ContactScheduledCall.make contact: @contact_c, scheduled_call: scheduled_call, last_called_at: c_last_called

        scheduled_call.channel.should_receive(:call).with(@contact_b.first_address, anything).ordered
        scheduled_call.channel.should_receive(:call).with(@contact_c.first_address, anything).ordered
        scheduled_call.channel.should_receive(:call).with(@contact_a.first_address, anything).ordered

        scheduled_call.make_calls from, to
      end

      it 'should call contacts that were never called first' do
        a_last_called = Time.utc(2014, 12, 04)
        b_last_called = a_last_called - 7.day

        ContactScheduledCall.make contact: @contact_a, scheduled_call: scheduled_call, last_called_at: a_last_called
        ContactScheduledCall.make contact: @contact_b, scheduled_call: scheduled_call, last_called_at: b_last_called

        scheduled_call.channel.should_receive(:call).with(@contact_c.first_address, anything).ordered
        scheduled_call.channel.should_receive(:call).with(@contact_b.first_address, anything).ordered
        scheduled_call.channel.should_receive(:call).with(@contact_a.first_address, anything).ordered

        scheduled_call.make_calls from, to
      end
    end
  end

  describe 'triggers' do
    it 'should schedule job when saved' do
      scheduled_call.should_receive(:schedule_job)
      scheduled_call.name = "new #{scheduled_call.name}"
      scheduled_call.save
    end

    it 'should delete existing jobs when updated' do
      scheduled_call.should_receive(:delete_jobs)
      scheduled_call.name = "new #{scheduled_call.name}"
      scheduled_call.save
    end

    it 'should delete existing jobs when destroyed' do
      scheduled_call.should_receive(:delete_jobs)
      scheduled_call.destroy
    end
  end

  describe 'next run at' do
    before :each do
      Timecop.freeze(Time.utc(2014, 12, 4, 13, 0, 0))

      scheduled_call.recurrence = IceCube::Schedule.new
      scheduled_call.recurrence_rule = IceCube::Rule.weekly.day(:monday)
    end

    after :each do
      Timecop.return
    end

    let(:tz) do
      ActiveSupport::TimeZone.new(scheduled_call.time_zone)
    end

    it 'should return next occurrence from now' do
      # this is next monday
      expected = Time.new(2014, 12, 8, 0, 0, 0, tz.formatted_offset)

      scheduled_call.next_run_at.should eq(expected)
    end

    it 'should return next occurrence from not before' do
      scheduled_call.not_before_enabled = true
      # a date between next monday and the other monday
      scheduled_call.not_before = Time.utc(2014, 12, 13)

      # this is other monday
      expected = Time.new(2014, 12, 15, 0, 0, 0, tz.formatted_offset)

      scheduled_call.next_run_at.should eq(expected)
    end

    it 'should not return a time in the past' do
      scheduled_call.time_zone = 'Auckland'
      scheduled_call.recurrence = IceCube::Schedule.new
      scheduled_call.recurrence_rule = IceCube::Rule.daily

      now = Time.utc(2014, 12, 4, 22, 0, 0)
      Timecop.freeze(now)

      scheduled_call.next_run_at.should > now
    end
  end

  describe 'schedule job' do
    let(:tz) do
      ActiveSupport::TimeZone.new(scheduled_call.time_zone)
    end

    let(:next_run_at_time) do
      Time.utc(2014, 12, 4, 13, 0, 0)
    end

    before(:each) do
      scheduled_call.stub(:next_run_at).and_return(next_run_at_time)
    end

    it 'should not schedule job if disabled' do
      scheduled_call.enabled = false

      Delayed::Job.should_not_receive(:enqueue)

      scheduled_call.schedule_job
    end

    it 'should not scheduled job if next occurrence if after not after' do
      scheduled_call.enabled = true
      scheduled_call.not_after_enabled = true
      scheduled_call.not_after = next_run_at_time.yesterday

      Delayed::Job.should_not_receive(:enqueue)

      scheduled_call.schedule_job
    end

    it 'should schedule job for next occurrence' do
      expected_run_at = next_run_at_time
      expected_from = Time.new(2014, 12, 4, 10, 0, 0, tz.formatted_offset)
      expected_to = Time.new(2014, 12, 4, 15, 0, 0, tz.formatted_offset)

      job = double('job')
      Jobs::ScheduledCallJob.should_receive(:new)
        .with(scheduled_call.id, expected_from, expected_to)
        .and_return(job)

      Delayed::Job.should_receive(:enqueue)
        .with(job, {scheduled_call_id: scheduled_call.id, run_at: expected_run_at})

      scheduled_call.schedule_job
    end

    describe 'time frame' do
      it 'should schedule job from same day to same day' do
        scheduled_call.from_time = 9 * 60 + 15 # 09:15
        scheduled_call.to_time = 19 * 60 + 30 # 19:30

        expected_from = Time.new(2014, 12, 4, 9, 15, 0, tz.formatted_offset)
        expected_to = Time.new(2014, 12, 4, 19, 30, 0, tz.formatted_offset)

        job = double('job', perform: nil)
        Jobs::ScheduledCallJob.should_receive(:new)
          .with(scheduled_call.id, expected_from, expected_to)
          .and_return(job)

        scheduled_call.schedule_job
      end


      it 'should schedule job from same day to next day' do
        scheduled_call.from_time = 16 * 60 + 30 # 16:30
        scheduled_call.to_time = 24 * 60 + 13 * 60 + 45 # 13:45 next day

        expected_from = Time.new(2014, 12, 4, 16, 30, 0, tz.formatted_offset)
        expected_to = Time.new(2014, 12, 5, 13, 45, 0, tz.formatted_offset)

        job = double('job', perform: nil)
        Jobs::ScheduledCallJob.should_receive(:new)
          .with(scheduled_call.id, expected_from, expected_to)
          .and_return(job)

        scheduled_call.schedule_job
      end

      it 'should schedule job from next day to next day' do
        scheduled_call.from_time = 24 * 60 + 15 * 60 # 15:00 next day
        scheduled_call.to_time = 24 * 60 + 21 * 60 + 45 # 21:45 next day

        expected_from = Time.new(2014, 12, 5, 15, 0, 0, tz.formatted_offset)
        expected_to = Time.new(2014, 12, 5, 21, 45, 0, tz.formatted_offset)

        job = double('job', perform: nil)
        Jobs::ScheduledCallJob.should_receive(:new)
          .with(scheduled_call.id, expected_from, expected_to)
          .and_return(job)

        scheduled_call.schedule_job
      end

      it 'should schedule job from next day to next day midnight' do
        scheduled_call.from_time = 24 * 60 # 00:00 next day
        scheduled_call.to_time = 2 * 24 * 60 # 24:00 next day (ie next next day)

        expected_from = Time.new(2014, 12, 5, 0, 0, 0, tz.formatted_offset)
        expected_to = Time.new(2014, 12, 6, 0, 0, 0, tz.formatted_offset)

        job = double('job', perform: nil)
        Jobs::ScheduledCallJob.should_receive(:new)
          .with(scheduled_call.id, expected_from, expected_to)
          .and_return(job)

        scheduled_call.schedule_job
      end
    end

  end

end
