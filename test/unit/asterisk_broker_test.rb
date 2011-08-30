require 'test_helper'

class AsteriskBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = Asterisk::Broker.new
    @broker.asterisk_client = mock('asterisk_client')
    @channel = Channel.make :kind => 'sip2sip'
  end

  context "call" do
    setup do
      @queued_call = @channel.queued_calls.make
    end

    should "call ok" do
      @broker.asterisk_client.expects(:error?).returns(false)
      @broker.asterisk_client.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@queued_call.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@channel.id},#{@queued_call.call_log.id}",
        :async => true,
        :actionid => @queued_call.call_log.id
      }).returns(:response => 'OK')

      result = @broker.call @queued_call
      assert_nil result
    end

    should "call fails on asterisk_client error" do
      @broker.asterisk_client.expects(:error?).returns(true)

      ex = assert_raise(RuntimeError) { @broker.call @queued_call }
      assert_match /not available/, ex.message
    end

    should "call fails on originate error" do
      @broker.asterisk_client.expects(:error?).returns(false)
      @broker.asterisk_client.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@queued_call.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@channel.id},#{@queued_call.call_log.id}",
        :async => true,
        :actionid => @queued_call.call_log.id
      }).returns(:response => 'Error', :message => 'Oops')

      ex = assert_raise(RuntimeError) { @broker.call @queued_call }
      assert_equal 'Oops', ex.message
    end
  end
end
