require 'test_helper'

class AmiProtocolTest < ActiveSupport::TestCase
  test "send" do
    ami = AmiProtocol.new nil

    seq = sequence('seq')
    ami.expects(:send_data).with("action: login\n").in_sequence(seq)
    ami.expects(:send_data).with("username: foo\n").in_sequence(seq)
    ami.expects(:send_data).with("secret: bar\n").in_sequence(seq)
    ami.expects(:send_data).with("\n").in_sequence(seq)

    Fiber.new { ami.login :username => 'foo', :secret => 'bar' }.resume
  end

  test "receive response" do
    ami = AmiProtocol.new nil

    ami.expects(:resume_fiber_with).with({:response => 'Success', :actionid => 'sarasa', :message => 'something'})

    ami.receive_line("Asterisk Call Manager/1.1\n")
    ami.receive_line("Response: Success\n")
    ami.receive_line("ActionID: sarasa\n")
    ami.receive_line("Message: something\n")
    ami.receive_line("\n")
  end

  test "receive event" do
    ami = AmiProtocol.new nil

    ami.expects(:receive_event).with({:event => 'Hangup', :actionid => 'sarasa', :message => 'something'})

    ami.receive_line("Asterisk Call Manager/1.1\n")
    ami.receive_line("Event: Hangup\n")
    ami.receive_line("ActionID: sarasa\n")
    ami.receive_line("Message: something\n")
    ami.receive_line("\n")
  end
end
