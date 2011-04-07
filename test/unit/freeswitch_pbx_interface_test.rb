require 'test_helper'

class FreeswitchPbxInterfaceTest < ActiveSupport::TestCase
  setup do
    @interface = Freeswitch::PbxInterface.new 1
    @interface.pbx = mock('pbx')
  end

  test "call ok" do
    @interface.pbx.expects(:error?).returns(false)
    @interface.pbx.expects(:command).with("bgapi originate {verboice_channel_id=1,verboice_call_log_id=2}user/1000 '&socket(localhost:#{Freeswitch::OutboundListener::Port} sync full)'")

    result = @interface.call 'user/1000', 1, 2
    assert_nil result
  end

  test "call fails on pbx error" do
    @interface.pbx.expects(:error?).returns(true)

    ex = assert_raise(RuntimeError) { @interface.call 'user/1000', 1, 2 }
    assert_match /not available/, ex.message
  end
end
