require 'test_helper'

class FreeswitchBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = Freeswitch::Broker.new
    @broker.freeswitch_client = mock 'freeswitch_client'
    @channel = Channel.make :kind => 'sip2sip'
    @queued_call = @channel.queued_calls.make
  end

  test "call ok" do
    @broker.freeswitch_client.expects(:error?).returns(false)
    @broker.freeswitch_client.expects(:command).with("bgapi originate {verboice_channel_id=#{@channel.id},verboice_call_log_id=#{@queued_call.call_log.id}}#{@queued_call.address} '&socket(localhost:#{Freeswitch::CallManager::Port} sync full)'")

    result = @broker.call @queued_call
    assert_nil result
  end

  test "call fails on pbx error" do
    @broker.freeswitch_client.expects(:error?).returns(true)

    ex = assert_raise(RuntimeError) { @broker.call @queued_call }
    assert_match /not available/, ex.message
  end
end
