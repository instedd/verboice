require 'spec_helper'

describe Asterisk::AmiProtocol do
  before(:each) do
    @ami = Asterisk::AmiProtocol.new nil
  end

  context "send" do
    it "send" do
      @ami.should_receive(:send_data).with("action: login\n").ordered
      @ami.should_receive(:send_data).with("username: foo\n").ordered
      @ami.should_receive(:send_data).with("secret: bar\n").ordered
      @ami.should_receive(:send_data).with("\n").ordered

      Fiber.new { @ami.login :username => 'foo', :secret => 'bar' }.resume
    end
  end

  context "receive" do
    it "receive response" do
      @ami.should_receive(:resume_fiber_with).with({:response => 'Success', :actionid => 'sarasa', :message => 'something'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Response: Success\n")
      @ami.receive_line("ActionID: sarasa\n")
      @ami.receive_line("Message: something\n")
      @ami.receive_line("\n")
    end

    it "receive response ignores if : not found" do
      @ami.should_receive(:resume_fiber_with).with({:response => 'Success'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Response: Success\n")
      @ami.receive_line(" -- END COMMAND -- \n")
      @ami.receive_line("\n")
    end

    it "receive event" do
      @ami.should_receive(:receive_event).with({:event => 'Hangup', :actionid => 'sarasa', :message => 'something'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Event: Hangup\n")
      @ami.receive_line("ActionID: sarasa\n")
      @ami.receive_line("Message: something\n")
      @ami.receive_line("\n")
    end
  end
end
