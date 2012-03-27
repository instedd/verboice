require 'spec_helper'

describe DialCommand do
  before(:each) do
    BaseBroker.instance = mock('broker')
  end

  it "run" do
    dial = DialCommand.new :number => '1234'
    session = Session.new :channel => Channel.make

    BaseBroker.instance.should_receive(:get_dial_address).with(session.channel, '1234').and_return('SIP/1234')
    session.call_log = CallLog.make
    session.pbx = mock('pbx')
    session.pbx.should_receive(:dial).with('SIP/1234', {}).and_return(:completed)
    dial.run session
    session[:dial_status].should == :completed
  end

  it "run with channel" do
    account = Account.make
    channel = Channel.make :account => account
    session = Session.new :channel => Channel.make(:account => account)
    dial = DialCommand.new :number => '1234', :channel => channel.name

    BaseBroker.instance.should_receive(:get_dial_address).with(channel, '1234').and_return('SIP/1234')
    session.call_log = CallLog.make
    session.pbx = mock('pbx')
    session.pbx.should_receive(:dial).with('SIP/1234', {})
    dial.run session
  end

  it "run with custom caller id" do
    dial = DialCommand.new :number => '1234', :caller_id => 'foo'
    session = Session.new :channel => Channel.make

    BaseBroker.instance.should_receive(:get_dial_address).with(session.channel, '1234').and_return('SIP/1234')
    session.call_log = CallLog.make
    session.pbx = mock('pbx')
    session.pbx.should_receive(:dial).with('SIP/1234', {:caller_id => 'foo'}).and_return(:completed)
    dial.run session
  end
end
