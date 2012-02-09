require 'spec_helper'

describe AnswerCommand do
  it "runs" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.should_receive(:answer)
    session.should_receive(:info).with('Answer')
    AnswerCommand.new.run session
  end
end
