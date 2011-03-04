require 'test_helper'

class AnswerCommandTest < ActiveSupport::TestCase
  test "run" do
    context = mock('context')
    context.expects(:answer)
    AnswerCommand.new.run context
  end
end
