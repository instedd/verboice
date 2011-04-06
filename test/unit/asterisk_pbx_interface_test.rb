require 'test_helper'

class AsteriskPbxInterfaceTest < ActiveSupport::TestCase
  setup do
    @interface = Asterisk::PbxInterface.new 1
    @interface.pbx = mock('pbx')
    @channel = Channel.make :kind => 'sip2sip'
  end

  context "call" do
    should "call ok" do
      @interface.pbx.expects(:error?).returns(false)
      @interface.pbx.expects(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/1234",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},#{@channel.id},2",
        :async => true,
        :actionid => 2
      }).returns(:response => 'OK')

      result = @interface.call '1234', @channel.id, 2
      assert_nil result
    end

    should "call fails on pbx error" do
      @interface.pbx.expects(:error?).returns(true)

      ex = assert_raise(RuntimeError) { @interface.call '1234', 1, 2 }
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

      ex = assert_raise(RuntimeError) { @interface.call '1234', @channel.id, 2 }
      assert_equal 'Oops', ex.message
    end
  end
end
