require 'test_helper'

class AnswerCommandTest < ActiveSupport::TestCase
  test "run" do
    session = Session.new :pbx => mock('pbx')
    session.pbx.expects(:answer)
    AnswerCommand.new.run session
  end
end
