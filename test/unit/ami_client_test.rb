class FastAGIProtocol; end # Because this is in lib/bathphone it doesn't get loaded...

require 'test_helper'

class AmiClientTest < ActiveSupport::TestCase
  setup do
    @ami = Asterisk::AmiClient.new 1
  end

  test "receive event originate response failure fails call log" do
    call_log = CallLog.make

    @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => call_log.id.to_s

    call_log.reload
    assert_match /Failed to establish the communication/, call_log.details
    assert_equal :failed, call_log.state
  end

  test "receive originate response without failure ignores it" do
    call_log = CallLog.make :state => :active

    @ami.receive_event :event => 'OriginateResponse', :response => 'Success', :actionid => call_log.id.to_s

    call_log.reload
    assert_equal :active, call_log.state
  end

  test "receive other failure event ignores it" do
    call_log = CallLog.make :state => :active

    @ami.receive_event :event => 'SomeOtherEvent', :response => 'Failure', :actionid => call_log.id.to_s

    call_log.reload
    assert_equal :active, call_log.state
  end
end

