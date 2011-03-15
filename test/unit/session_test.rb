require 'test_helper'

class SessionTest < ActiveSupport::TestCase
  setup do
    @pbx = mock('pbx')
    @session = Session.new :pbx => @pbx
  end

  test "run no args command" do
    @pbx.expects(:foo)
    @session.commands = [:no_args]
    @session.run
  end

  test "run args command" do
    @pbx.expects(:bar).with(1)
    @session.commands = [:args => {:n => 1}]
    @session.run
  end

  test "run and push commands" do
    @pbx.expects(:foo)
    @session.commands = [:push => :no_args]
    @session.run
  end
end

class NoArgsCommand
  def run(session)
    session.pbx.foo
  end
end

class ArgsCommand
  def initialize(options)
    @n = options[:n]
  end

  def run(session)
    session.pbx.bar @n
  end
end

class PushCommand
  def initialize(command)
    @command = command
  end

  def run(session)
    session.push_commands [@command]
  end
end
