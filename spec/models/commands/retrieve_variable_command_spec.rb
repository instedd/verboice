require 'spec_helper'

module Commands
  describe RetrieveVariableCommand do
    it "should retrieve a persisted variable" do

      persisted_variable = PersistedVariable.make name: 'foo'

      contact = persisted_variable.contact

      project = Project.make account: contact.account

      call_flow = CallFlow.make project: project

      call_log = CallLog.make call_flow: call_flow

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next
      cmd.run(session).should == :next
      session.eval('var_foo').should eq(persisted_variable.value.to_i)
    end

    it "should set to nil if the contact doesn't exist" do
      call_log = CallLog.make

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => '1234xxxx'

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next
      cmd.run(session).should == :next
      session.eval('var_foo').should eq(nil)
    end

    it "should set to nil if the variable doesn't exist" do

      contact = Contact.make
      project = Project.make account: contact.account
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next
      cmd.run(session).should == :next
      session.eval('var_foo').should eq(nil)
    end
  end
end