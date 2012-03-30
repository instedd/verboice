require 'spec_helper'

describe Freeswitch::Broker do
  before(:each) do
    @broker = Freeswitch::Broker.new
    @broker.freeswitch_client = mock 'freeswitch_client'
    @channel = Channel.make :kind => 'custom'
    @queued_call = @channel.queued_calls.make
  end

  it "call ok" do
    @broker.freeswitch_client.should_receive(:error?).and_return(false)
    @broker.freeswitch_client.should_receive(:command).with("bgapi originate {verboice_channel_id=#{@channel.id},verboice_call_log_id=#{@queued_call.call_log.id}}#{@queued_call.address} '&socket(localhost:#{Freeswitch::CallManager::Port} sync full)'")

    result = @broker.call @queued_call
    result.should == nil
  end

  it "call fails on pbx error" do
    @broker.freeswitch_client.should_receive(:error?).and_return(true)

    ex = assert_raise(RuntimeError) { @broker.call @queued_call }
    assert_match /not available/, ex.message
  end
end
