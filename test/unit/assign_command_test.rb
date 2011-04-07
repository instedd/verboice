require 'test_helper'

class AssignCommandTest < ActiveSupport::TestCase
  setup do
    @session = Session.new :pbx => mock('pbx')
  end

  test "assigns" do
    cmd = AssignCommand.new :name => 'foo', :expr => '1 + 2'
    cmd.run @session

    assert_equal 3, @session['foo']
  end
end
