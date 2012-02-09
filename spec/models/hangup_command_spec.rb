require 'spec_helper'

describe HangupCommand do
  it "run" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.should_receive(:hangup)
    session.should_receive(:info).with('Hangup')
    HangupCommand.new.run session
  end
end
