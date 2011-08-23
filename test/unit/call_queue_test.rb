require 'test_helper'

class CallQueueTest < ActiveSupport::TestCase
  
  setup do
    @channel = Channel.make
    @call_log = CallLog.make
  end
  
  test "should enqueue call" do
    CallQueue.enqueue @channel, @call_log, 'address'
    call = CallQueue.first
    assert_not_nil call
    assert_equal @channel, call.channel
    assert_equal @call_log, call.call_log
    assert_equal 'address', call.address
  end
  
  test "should poll nil if no call is queued" do
    assert_nil CallQueue.poll @channel
  end
  
  test "should poll queued call" do
    CallQueue.enqueue @channel, @call_log, 'address'
    call = CallQueue.poll @channel
    assert_not_nil call
    assert_equal [], CallQueue.all
  end
  
end
