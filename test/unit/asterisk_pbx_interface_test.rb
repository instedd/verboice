require 'test_helper'

class AsteriskPbxInterfaceTest < ActiveSupport::TestCase
  setup do
    @interface = Asterisk::PbxInterface.new 1
    @interface.pbx = mock('pbx')
    @channel = Channel.make :kind => 'sip2sip'
  end
  
  context "call from queue" do
    
    setup do
      @call = mock('call')
      @call.stubs(:address).returns('address')
      @call.stubs(:call_log).returns('call_log')
      @call.stubs(:channel).returns(@channel)
      Channel.stubs(:find).with{|id| id == @channel.id}.returns(@channel)
    end
    
    should "call if there is a queued call" do
      @channel.expects(:can_call?).returns(true)
      CallQueue.expects(:poll).returns(@call)
      @interface.expects(:call).with do |address, channel, call_log|
        address == @call.address && channel == @call.channel && call_log == @call.call_log
      end
      
      @interface.try_call_from_queue @channel.id
    end
    
    should "not call if channel is unavailable" do
      @channel.expects(:can_call?).returns(false)
      @interface.expects(:call).never
      
      @interface.try_call_from_queue @channel.id
    end
    
    should "not call if there is no pending call" do
      @channel.expects(:can_call?).returns(true)
      CallQueue.expects(:poll).returns(nil)
      @interface.expects(:call).never
      
      @interface.try_call_from_queue @channel.id
    end
    
  end

  context "call" do
    
    setup do
      @call_log = mock('call_log')
      @call_log.stubs(:id).returns(2)
      @call_log.expects(:start)
    end
    
    should "call ok" do
      @interface.pbx.expects(:error?).returns(false)
      @interface.pbx.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/1234",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},#{@channel.id},2",
        :async => true,
        :actionid => 2
      }).returns(:response => 'OK')

      result = @interface.call '1234', @channel, @call_log
      assert_nil result
    end

    should "call fails on pbx error" do
      @interface.pbx.expects(:error?).returns(true)

      ex = assert_raise(RuntimeError) { @interface.call '1234', @channel, @call_log }
      assert_match /not available/, ex.message
    end

    should "call fails on originate error" do
      @interface.pbx.expects(:error?).returns(false)
      @interface.pbx.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/1234",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},#{@channel.id},2",
        :async => true,
        :actionid => 2
      }).returns(:response => 'Error', :message => 'Oops')

      ex = assert_raise(RuntimeError) { @interface.call '1234', @channel, @call_log }
      assert_equal 'Oops', ex.message
    end

  end
end
