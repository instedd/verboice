require 'test_helper'

class AsteriskBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = Asterisk::Broker.new
    $asterisk_client = mock('asterisk_client')
    @channel = Channel.make :kind => 'sip2sip'
  end

  context "call" do
    setup do
      @session = Session.new :channel => @channel, :address => 'Foo'
    end

    should "call ok" do
      $asterisk_client.expects(:error?).returns(false)
      $asterisk_client.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).returns(:response => 'OK')

      result = @broker.call @session
      assert_nil result
    end

    should "call fails on asterisk_client error" do
      $asterisk_client.expects(:error?).returns(true)

      ex = assert_raise(RuntimeError) { @broker.call @session }
      assert_match /not available/, ex.message
    end

    should "call fails on originate error" do
      $asterisk_client.expects(:error?).returns(false)
      $asterisk_client.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).returns(:response => 'Error', :message => 'Oops')

      ex = assert_raise(RuntimeError) { @broker.call @session }
      assert_equal 'Oops', ex.message
    end
  end
end
