require 'test_helper'

class CallLogTest < ActiveSupport::TestCase
  should belong_to(:account)
  should belong_to(:application)
  should belong_to(:channel)

  should validate_presence_of(:account)
  should validate_presence_of(:application)
  should validate_presence_of(:channel)

  test "call log structured details" do
    log = CallLog.new :details => <<EOF
I 0.13 Answer
T 12.25 Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits=
T 123.48 Callback returned: http://localhost:4567/guess.mp3
and some other text... possibly...
EOF
    details = log.structured_details
    assert_equal 3, details.length
    assert_equal({:severity => :info, :time => '0.13', :text => 'Answer'}, details[0])
    assert_equal({:severity => :trace, :time => '12.25', :text => 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits='}, details[1])
    assert_equal({:severity => :trace, :time => '123.48', :text => 'Callback returned: http://localhost:4567/guess.mp3
and some other text... possibly...'}, details[2])
  end

  test "create for application assigns account" do
    chan = Channel.make
    app = chan.application
    call_log = app.call_logs.create! :channel => chan
    assert_equal app.account_id, call_log.account_id
  end
  
  test "save started at" do
    call_log = CallLog.make
    assert_nil call_log.started_at
    
    time = Time.now
    Time.stubs(:now).returns(time)
    
    call_log.start
    assert_equal time, call_log.started_at
  end
  
end
