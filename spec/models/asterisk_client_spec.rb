require 'spec_helper'

describe Asterisk::Client do
  context "receiving events" do
    before(:each) do
      @ami = Asterisk::Client.new 1
      @session = Session.new :call_log => CallLog.make
    end

    it "receive event originate response failure fails with busy" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :busy)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '5'
    end

    it "receive event originate response failure fails with no answer" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :no_answer)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '3'
    end

    it "receive event originate response failure fails with generic failure" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :failed)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => 'X'
    end

    it "receive originate response without failure ignores it" do
      @ami.receive_event :event => 'OriginateResponse', :response => 'Success', :actionid => @session.id.to_s

      @session.call_log.reload
      @session.call_log.state.should == :active
    end

    it "receive other failure event ignores it" do
      @ami.receive_event :event => 'SomeOtherEvent', :response => 'Failure', :actionid => @session.id.to_s

      @session.call_log.reload
      @session.call_log.state.should == :active
    end
  end
end

