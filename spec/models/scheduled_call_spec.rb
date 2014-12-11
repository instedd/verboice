require 'spec_helper'

describe ScheduledCall do
  it { should belong_to(:project) }
  it { should belong_to(:call_flow) }
  it { should belong_to(:channel) }
  it { should belong_to(:schedule) }

  it { should validate_presence_of(:name) }
  it { should validate_presence_of(:project) }
  it { should validate_presence_of(:call_flow) }
  it { should validate_presence_of(:channel) }
  it { should validate_presence_of(:schedule) }
  it { should validate_presence_of(:frequency) }
  it { should validate_presence_of(:time_zone) }

  let(:scheduled_call) { ScheduledCall.make }

  it "should find matched contacts" do
    scheduled_call.filters = {foo: 'bar'}
    finder = double('finder')
    ContactsFinder.should_receive(:for).with(scheduled_call.project).and_return(finder)
    finder.should_receive(:find).with(scheduled_call.filters).and_return([1,2,3])

    scheduled_call.matched_contacts.should eq([1,2,3])
  end

  describe 'calls' do
    before :each do
      @contact_a = Contact.make project: scheduled_call.project
      @contact_b = Contact.make project: scheduled_call.project

      scheduled_call.stub(:matched_contacts).and_return([@contact_a, @contact_b])
    end

    let(:expected_options) do
      {
        account: scheduled_call.project.account,
        schedule_id: scheduled_call.schedule_id,
        time_zone: scheduled_call.time_zone,
        call_flow_id: scheduled_call.call_flow_id,
        project_id: scheduled_call.project_id
      }
    end

    it "should make calls" do
      scheduled_call.channel.should_receive(:call).with(@contact_a.first_address, expected_options)
      scheduled_call.channel.should_receive(:call).with(@contact_b.first_address, expected_options)

      scheduled_call.make_calls
    end

    it "should not make calls if disabled" do
      scheduled_call.enabled = false

      scheduled_call.channel.should_not_receive(:call)

      scheduled_call.make_calls
    end

    it "should make calls with not_before if selected" do
      time_zone = ActiveSupport::TimeZone.new scheduled_call.time_zone
      not_before_local = time_zone.local 2014, 12, 04, 12

      scheduled_call.not_before_enabled = true
      scheduled_call.not_before = not_before_local.utc

      options = expected_options.merge({not_before: not_before_local})

      scheduled_call.channel.should_receive(:call).with(@contact_a.first_address, options)
      scheduled_call.channel.should_receive(:call).with(@contact_b.first_address, options)

      scheduled_call.make_calls
    end
  end
end
