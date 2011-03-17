require 'test_helper'

class HangupCommandTest < ActiveSupport::TestCase
  test "run" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.expects(:hangup)
    session.expects(:info).with('Hangup')
    HangupCommand.new.run session
  end
end
