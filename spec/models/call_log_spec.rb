require 'spec_helper'

describe CallLog do
  it { should belong_to(:account) }
  it { should belong_to(:project) }
  it { should belong_to(:channel) }

  it { should validate_presence_of(:account) }
  it { should validate_presence_of(:project) }
  it { should validate_presence_of(:channel) }

  it "call log structured details" do
    log = CallLog.new :details => <<EOF
I 0.13 Answer
T 12.25 Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits=
T 123.48 Callback returned: http://localhost:4567/guess.mp3
and some other text... possibly...
EOF
    details = log.structured_details
    details.length.should == 3
    assert_equal({:severity => :info, :time => Time.at('0.13'.to_f).utc, :text => 'Answer'}, details[0])
    assert_equal({:severity => :trace, :time => Time.at('12.25'.to_f).utc, :text => 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits='}, details[1])
    assert_equal({:severity => :trace, :time => Time.at('123.48'.to_f).utc, :text => 'Callback returned: http://localhost:4567/guess.mp3
and some other text... possibly...'}, details[2])
  end

  it "create for project assigns account" do
    channel = Channel.make
    call_flow = channel.call_flow
    call_log = call_flow.call_logs.create! :channel => channel
    call_log.account_id.should == call_flow.account.id
  end

  it "save started at when starting an outgoing call" do
    call_log = CallLog.make
    call_log.started_at.should == nil

    time = Time.now
    Time.stub(:now => time)

    call_log.start_outgoing '1234'
    call_log.started_at.should == time
    assert_match /Calling 1234/, call_log.details
    call_log.state.should == :active
    call_log.address.should == '1234'
  end
end
