require 'spec_helper'

describe Commands::HangupCommand do
  it "run" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.should_receive(:hangup)
    session.should_receive(:info).with('Hangup')
    Commands::HangupCommand.new.run session
  end
end
