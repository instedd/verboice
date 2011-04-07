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

  test "run command with class" do
    @pbx.expects(:foo)
    @session.commands = [NoArgsCommand.new]
    @session.run
  end

  test "run and push commands" do
    @pbx.expects(:foo)
    @session.commands = [:push => :no_args]
    @session.run
  end

  test "saves log" do
    @pbx.expects(:answer)
    @session.call_log = CallLog.make
    @session.commands = [:answer]
    @session.run

    logs = CallLog.all
    assert_equal 1, logs.length
    assert_match /^I.*?Answer/, logs.first.details
  end

  context "answering machine detection" do
    setup do
      @call_log = mock('call_log')
      @session.call_log = @call_log
    end

    should "run ok if call is incoming" do
      @call_log.expects(:outgoing?).returns(false)
      @call_log.expects(:finish).with(:completed)
      @session.commands = []
      @session.run
    end

    should "run ok if call is outgoing but not an answeing machine" do
      @pbx.expects(:is_answering_machine?).returns(false)
      @call_log.expects(:outgoing?).returns(true)
      @call_log.expects(:finish).with(:completed)
      @session.commands = []
      @session.run
    end

    should "fail if call is outgoing and an answeing machine" do
      @pbx.expects(:is_answering_machine?).returns(true)
      @call_log.expects(:outgoing?).returns(true)
      @call_log.expects(:error).with('Answering machine detected')
      @call_log.expects(:finish).with(:failed)
      @session.commands = []
      @session.run
    end
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
