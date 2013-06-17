# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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

  it "expands vars" do
    @session['var_foo'] = 'world'
    @session.expand_vars('hello {foo}').should eq('hello world')
  end

  it "creates an anonymous contact when there is no address" do
    @session.contact.should_not be_nil
    @session.contact.should be_anonymous
    @session.contact.addresses.count.should eq(1)
  end

  it "creates a contact for the given address" do
    @session.address = '123'

    @session.contact.should_not be_nil
    @session.contact.should_not be_anonymous
    @session.contact.addresses.count.should eq(1)
    @session.contact.addresses.first.address.should eq('123')
  end

  it "find a pre-existing contact for the given address" do
    contact = @session.project.contacts.new
    contact.addresses.build address: '123'
    contact.save!

    @session.address = '123'

    @session.contact.should_not be_nil
    @session.contact.should eq(contact)
  end

  context "javascript evaluation" do
    it "alert calls info" do
      @session.should_receive(:info).with('foo')
      @session.eval %q(alert('foo'))
    end
  end

  context "sending status notification for an project" do
    let(:project) { Project.make(:status_callback_url => 'http://foo') }

    def apply_authentication_to_project(user = "", password = "")
      project.status_callback_url_user = user
      project.status_callback_url_password = password
      project.save
    end

    before do
      @session.call_log.project = project
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
    it "run ok if call is incoming" do
      @session.call_log = CallLog.make direction: :incoming
      @session.commands = nil
      @session.run
    end

    it "run ok if call is outgoing but not an answeing machine" do
      @session.call_log = CallLog.make direction: :outgoing
      @pbx.should_receive(:is_answering_machine?).and_return(false)
      @session.commands = nil
      @session.run
    end

    it "fail if call is outgoing and an answeing machine" do
      @session.call_log = CallLog.make direction: :outgoing
      @pbx.should_receive(:is_answering_machine?).and_return(true)
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
