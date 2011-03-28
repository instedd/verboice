require 'test_helper'

class AsteriskPbxInterfaceTest < ActiveSupport::TestCase
  setup do
    @interface = Asterisk::PbxInterface.new 1
    @interface.pbx = mock('pbx')
  end

  test "call ok" do
    @interface.pbx.expects(:error?).returns(false)
    @interface.pbx.expects(:originate).with({
      :channel => 'SIP/1000',
      :application => 'AGI',
      :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},1,2",
      :async => true,
      :actionid => 2
    }).returns(:response => 'OK')

    result = @interface.call 'SIP/1000', 1, 2
    assert_nil result
  end

  test "call fails on pbx error" do
    @interface.pbx.expects(:error?).returns(true)

    begin
      @interface.call 'SIP/1000', 1, 2
      fail 'Expected exception to be thrown'
    rescue => ex
      assert_match /not available/, ex.message
    end
  end

  test "call fails on originate error" do
    @interface.pbx.expects(:error?).returns(false)
    @interface.pbx.expects(:originate).with({
      :channel => 'SIP/1000',
      :application => 'AGI',
      :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},1,2",
      :async => true,
      :actionid => 2
    }).returns(:response => 'Error', :message => 'Oops')

    begin
      @interface.call 'SIP/1000', 1, 2
      fail 'Expected exception to be thrown'
    rescue => ex
      assert_equal 'Oops', ex.message
    end
  end
end
