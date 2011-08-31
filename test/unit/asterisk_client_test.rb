require 'test_helper'

class AsteriskClientTest < ActiveSupport::TestCase
  context "receiving events" do
    setup do
      @ami = Asterisk::Client.new 1
      @session = Session.new :call_log => CallLog.make
    end

    should "receive event originate response failure fails call log" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.expects(:finish_session_with_error).with(@session.id, 'Failed to establish the communication')

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s
    end

    should "receive originate response without failure ignores it" do
      @ami.receive_event :event => 'OriginateResponse', :response => 'Success', :actionid => @session.id.to_s

      @session.call_log.reload
      assert_equal :active, @session.call_log.state
    end

    should "receive other failure event ignores it" do
      @ami.receive_event :event => 'SomeOtherEvent', :response => 'Failure', :actionid => @session.id.to_s

      @session.call_log.reload
      assert_equal :active, @session.call_log.state
    end
  end
end

