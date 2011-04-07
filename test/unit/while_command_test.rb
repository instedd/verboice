require 'test_helper'

class WhileCommandTest < ActiveSupport::TestCase
  setup do
    @session = Session.new :pbx => mock('pbx')
  end

  test "while when true" do
    @session['i'] = 0

    do_commands = {:assign => {:name => :i, :expr => 'i + 1'}}
    cmd = WhileCommand.new :condition => 'i == 0', :do => do_commands

    @session.expects(:push_commands).with([do_commands, cmd])

    cmd.run @session
  end

  test "while when false" do
    @session['i'] = 0

    do_commands = {:assign => {:name => :i, :expr => 'i + 1'}}
    cmd = WhileCommand.new :condition => 'i != 0', :do => do_commands

    @session.expects(:push_commands).never

    cmd.run @session
  end
end
