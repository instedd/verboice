require 'spec_helper'

describe Session do
  before(:each) do
    @pbx = mock('pbx')
    @session = Session.new :pbx => @pbx, :call_log => CallLog.make
  end

  it "run no args command" do
    @pbx.should_receive(:foo)
    @session.commands = [:no_args]
    @session.run
  end

  it "run args command" do
    @pbx.should_receive(:bar).with(1)
    @session.commands = [:args => {:n => 1}]
    @session.run
  end

  it "run command with class" do
    @pbx.should_receive(:foo)
    @session.commands = [NoArgsCommand.new]
    @session.run
  end

  it "run and push commands" do
    @pbx.should_receive(:foo)
    @session.commands = [:push => :no_args]
    @session.run
  end

  it "and_return id of call log" do
    @session.id.should_not be_nil
  end

  it "suspend session" do
    @session.commands = [:yield, :no_args]
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    @session.suspended.should_not be_nil
  end

  it "resume session" do
    @session.commands = [:yield]
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    @session.commands = [:no_args]
    @pbx.should_receive(:foo)
    @session.resume
  end

  it "send status notification" do
    @session.application = Application.new :status_callback_url => 'http://foo'
    @pbx.should_receive(:caller_id).and_return('999')
    expect_em_http :get, 'http://foo', :with => { :query => { :CallSid => @session.call_log.id, :From => '999', :CallStatus => 'foo' }}, :callback => false, :errback => false
    @session.notify_status 'foo'
  end

  context "answering machine detection" do
    before(:each) do
      @call_log = mock('call_log')
      @session.call_log = @call_log
    end

    it "run ok if call is incoming" do
      @call_log.should_receive(:outgoing?).and_return(false)
      @session.commands = []
      @session.run
    end

    it "run ok if call is outgoing but not an answeing machine" do
      @pbx.should_receive(:is_answering_machine?).and_return(false)
      @call_log.should_receive(:outgoing?).and_return(true)
      @session.commands = []
      @session.run
    end

    it "fail if call is outgoing and an answeing machine" do
      @pbx.should_receive(:is_answering_machine?).and_return(true)
      @call_log.should_receive(:outgoing?).and_return(true)
      @session.commands = []
      begin
        @session.run
        fail "Exception expected to be raised"
      rescue RuntimeError => ex
        ex.message.should == "Answering machine detected"
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
