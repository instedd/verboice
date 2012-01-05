require 'test_helper'

class AsteriskClientTest < ActiveSupport::TestCase
  context "receiving events" do
    setup do
      @ami = Asterisk::Client.new 1
      @session = Session.new :call_log => CallLog.make
    end

    should "receive event originate response failure fails with busy" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.expects(:call_rejected).with(@session.id, :busy)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '5'
    end

    should "receive event originate response failure fails with no answer" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.expects(:call_rejected).with(@session.id, :no_answer)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '3'
    end

    should "receive event originate response failure fails with generic failure" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.expects(:call_rejected).with(@session.id, :failed)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => 'X'
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

