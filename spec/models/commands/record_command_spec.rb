require 'spec_helper'

module Commands
  describe RecordCommand do
    # it "should create a recorded audio linking the saved audio file to the call log and contact" do
    #   # contact = Contact.make
    #   # application = Application.make account: contact.account
    #   # call_log = CallLog.make application: application
    #   #
    #   # session = Session.new :pbx => mock('pbx'), :call_log => call_log
    #   # session.stub :address => contact.address
    #   #
    #   # cmd = PersistVariableCommand.new 'foo', 2
    #   # cmd.next = :next
    #   # cmd.run(session).should == :next
    #   #
    #   # PersistedVariable.all.size.should eq(1)
    #   # PersistedVariable.first.value.should eq('2')
    #   # PersistedVariable.first.name.should eq('foo')
    #   # PersistedVariable.first.contact.should eq(contact)
    # end
    #
    # it "should create a Contact if it doesn't exist" do
    #   # call_log = CallLog.make
    #   # session = Session.new :pbx => mock('pbx'), :call_log => call_log
    #   # session.stub :address => '1234xxx'
    #   #
    #   # cmd = PersistVariableCommand.new 'foo', 2
    #   # cmd.next = :next
    #   # cmd.run(session).should == :next
    #   # Contact.all.size.should eq(1)
    #   # Contact.first.address.should eq('1234xxx')
    #   # PersistedVariable.first.contact.should eq(Contact.first)
    # end
  end
end