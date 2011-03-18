require 'test_helper'

class AmiProtocolTest < ActiveSupport::TestCase
  test "send" do
    seq = sequence('seq')

    ami = AmiProtocol.new nil
    ami.expects(:send_data).with("action: login\n").in_sequence(seq)
    ami.expects(:send_data).with("username: foo\n").in_sequence(seq)
    ami.expects(:send_data).with("secret: bar\n").in_sequence(seq)
    ami.expects(:send_data).with("\n").in_sequence(seq)

    ami.login :username => 'foo', :secret => 'bar'
  end
end
