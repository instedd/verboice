require 'spec_helper'

module Commands
  describe PersistVariableCommand do
    it "should create a persisted variable storing a value with a given name" do
      contact = Contact.make
      application = Application.make account: contact.account
      call_log = CallLog.make application: application

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('2')
      PersistedVariable.first.name.should eq('foo')
      PersistedVariable.first.contact.should eq(contact)
    end

    it "should create a Contact if it doesn't exists" do
      call_log = CallLog.make
      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => '1234xxx'

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next
      Contact.last.address.should eq('1234xxx')
      PersistedVariable.first.contact.should eq(Contact.last)
    end
  end
end