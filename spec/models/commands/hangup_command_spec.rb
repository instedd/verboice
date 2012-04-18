require 'spec_helper'

describe Commands::HangupCommand do
  it "run" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.should_receive(:hangup)
    session.should_receive(:info).with('Hangup')
    cmd = Commands::HangupCommand.new
    cmd.next = :next
    cmd.run(session).should == :next
  end
end
