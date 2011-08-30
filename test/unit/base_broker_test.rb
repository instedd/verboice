require 'test_helper'

class BaseBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = BaseBroker.new
    @channel = mock 'channel'
    @queued_call = mock 'call'
  end

  context "notify queued call" do
    should "not call if there is no queued call" do
      @channel.expects(:poll_call).returns nil

      @broker.expects(:call).never
      @broker.notify_call_queued @channel
    end

    should "call if there is a queued call" do
      @channel.expects(:poll_call).returns @queued_call

      @broker.expects(:call).with @queued_call
      @broker.notify_call_queued @channel
    end
  end
end
