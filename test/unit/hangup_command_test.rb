require 'test_helper'

class HangupCommandTest < ActiveSupport::TestCase
  test "run" do
    context = mock('context')
    context.expects(:hangup)
    HangupCommand.new.run context
  end
end
