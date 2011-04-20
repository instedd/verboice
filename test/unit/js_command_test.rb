require 'test_helper'

class JsCommandTest < ActiveSupport::TestCase
  setup do
    @session = Session.new :pbx => mock('pbx')
  end

  test "answer" do
    @session.pbx.expects(:answer)

    cmd = JsCommand.new 'answer();'
    cmd.run @session
  end
end
