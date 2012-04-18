require 'spec_helper'

describe Commands::AnswerCommand do
  it "runs" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.should_receive(:answer)
    session.should_receive(:info).with('Answer')
    cmd = Commands::AnswerCommand.new
    cmd.next = :next
    cmd.run(session).should == :next
  end
end
