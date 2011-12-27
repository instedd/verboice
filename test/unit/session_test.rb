require 'test_helper'

class SessionTest < ActiveSupport::TestCase
  setup do
    @pbx = mock('pbx')
    @session = Session.new :pbx => @pbx, :call_log => CallLog.make
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

  test "returns id of call log" do
    assert_not_nil @session.id
  end

  test "suspend session" do
    @session.commands = [:yield, :no_args]
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    assert @session.suspended
  end

  test "resume session" do
    @session.commands = [:yield]
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    @session.commands = [:no_args]
    @pbx.expects(:foo)
    @session.resume
  end

  test "send status notification" do
    @session.application = Application.new :status_callback_url => 'http://foo'
    @pbx.expects(:caller_id).returns('999')
    expect_em_http :get, 'http://foo', :with => { :query => { :CallSid => @session.call_log.id, :From => '999', :CallStatus => 'foo' }}, :callback => false, :errback => false
    @session.notify_status 'foo'
  end

  context "answering machine detection" do
    setup do
      @call_log = mock('call_log')
      @session.call_log = @call_log
    end

    should "run ok if call is incoming" do
      @call_log.expects(:outgoing?).returns(false)
      @session.commands = []
      @session.run
    end

    should "run ok if call is outgoing but not an answeing machine" do
      @pbx.expects(:is_answering_machine?).returns(false)
      @call_log.expects(:outgoing?).returns(true)
      @session.commands = []
      @session.run
    end

    should "fail if call is outgoing and an answeing machine" do
      @pbx.expects(:is_answering_machine?).returns(true)
      @call_log.expects(:outgoing?).returns(true)
      @session.commands = []
      begin
        @session.run
        fail "Exception expected to be raised"
      rescue RuntimeError => ex
        assert_equal "Answering machine detected", ex.message
      end
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

class YieldCommand
  def run(session)
    Fiber.yield
  end
end
