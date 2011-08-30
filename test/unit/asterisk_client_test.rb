require 'test_helper'

class AsteriskClientTest < ActiveSupport::TestCase
  context "receiving events" do
    setup do
      @ami = Asterisk::Client.new 1
      @call_log = CallLog.make
    end

    should "receive event originate response failure fails call log" do
      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @call_log.id.to_s

      @call_log.reload
      assert_match /Failed to establish the communication/, @call_log.details
      assert_equal :failed, @call_log.state
    end

    should "receive originate response without failure ignores it" do
      @ami.receive_event :event => 'OriginateResponse', :response => 'Success', :actionid => @call_log.id.to_s

      @call_log.reload
      assert_equal :active, @call_log.state
    end

    should "receive other failure event ignores it" do
      @ami.receive_event :event => 'SomeOtherEvent', :response => 'Failure', :actionid => @call_log.id.to_s

      @call_log.reload
      assert_equal :active, @call_log.state
    end
  end
end

