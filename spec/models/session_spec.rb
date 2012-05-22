require 'spec_helper'

describe Session do
  before(:each) do
    @pbx = mock('pbx')
    @session = Session.new :pbx => @pbx, :call_log => CallLog.make
  end

  it "run no args command" do
    @pbx.should_receive(:foo)
    @session.commands = Commands::NoArgsCommand.new
    @session.run
  end

  it "run many commands" do
    @pbx.should_receive(:foo)
    @session.commands = Commands::NextCommand.new(Commands::NoArgsCommand.new)
    @session.run
  end

  it "and_return id of call log" do
    @session.id.should_not be_nil
  end

  it "suspend session" do
    @session.commands = Compiler.make { Yield(); NoArgs() }
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    @session.suspended.should_not be_nil
  end

  it "resume session" do
    @session.commands = Commands::YieldCommand.new
    f = Fiber.new do
      @session.run
    end
    f.resume
    @session.suspend
    f.resume

    @session.commands = Commands::NoArgsCommand.new
    @pbx.should_receive(:foo)
    @session.resume
  end

  context "sending status notification for an project" do
    let(:project) { Project.make(:status_callback_url => 'http://foo') }
    let(:call_flow) { CallFlow.make :project => project }

    def apply_authentication_to_project(user = "", password = "")
      project.status_callback_url_user = user
      project.status_callback_url_password = password
      project.save
    end

    before do
      @session.call_flow = call_flow
      @pbx.stub(:caller_id).and_return('999')
      @pbx.should_receive(:caller_id)
    end

    context "with status callback url authentication" do
      before do
        apply_authentication_to_project("user", "password")
      end

      it "should send a status notification with authentication" do
        expect_em_http :get, 'http://foo', :with => { :query => { :CallSid => @session.call_log.id, :From => '999', :CallStatus => 'foo' }, :head => {'authorization' => ['user', 'password']}}, :callback => false, :errback => false
        @session.notify_status 'foo'
      end
    end

    context "without status callback url authentication" do

      before do
        apply_authentication_to_project
      end

      it "should send a status notification without authentication" do
        expect_em_http :get, 'http://foo', :with => { :query => { :CallSid => @session.call_log.id, :From => '999', :CallStatus => 'foo' }}, :callback => false, :errback => false
        @session.notify_status 'foo'
      end
    end
  end

  context "answering machine detection" do
    before(:each) do
      @call_log = mock('call_log')
      @session.call_log = @call_log
    end

    it "run ok if call is incoming" do
      @call_log.should_receive(:outgoing?).and_return(false)
      @session.commands = nil
      @session.run
    end

    it "run ok if call is outgoing but not an answeing machine" do
      @pbx.should_receive(:is_answering_machine?).and_return(false)
      @call_log.should_receive(:outgoing?).and_return(true)
      @session.commands = nil
      @session.run
    end

    it "fail if call is outgoing and an answeing machine" do
      @pbx.should_receive(:is_answering_machine?).and_return(true)
      @call_log.should_receive(:outgoing?).and_return(true)
      @session.commands = nil
      begin
        @session.run
        fail "Exception expected to be raised"
      rescue RuntimeError => ex
        ex.message.should == "Answering machine detected"
      end
    end
  end
end

module Commands
  class NoArgsCommand < Command
    def run(session)
      session.pbx.foo
      super
    end
  end

  class ArgsCommand < Command
    def initialize(options)
      @n = options[:n]
    end

    def run(session)
      session.pbx.bar @n
      super
    end
  end

  class NextCommand < Command
    def initialize(command)
      self.next = command
    end

    def run(session)
      super
    end
  end

  class YieldCommand < Command
    def run(session)
      Fiber.yield
      super
    end
  end
end