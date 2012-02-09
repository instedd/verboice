require 'test_helper'

class ApiControllerTest < ActionController::TestCase
  include Devise::TestHelpers

  def setup
    @account = Account.make
    sign_in @account
  end

  test "call" do
    call_log = CallLog.make
    @controller.current_account.expects(:call).returns(call_log)
    get :call, :address => 'foo', :callback => 'bar'
    result = JSON.parse(@response.body)
    assert_equal call_log.id, result['call_id']
  end

  test "call state" do
    call_log = CallLog.make :application => Application.make(:account => @controller.current_account)
    get :call_state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    assert_equal call_log.id, result['call_id']
    assert_equal call_log.state.to_s, result['state']
  end

  test "schedule call in the future" do
    channel = @account.channels.make
    BrokerClient.expects(:notify_call_queued).with(channel.id)
    time = Time.now.utc + 1.hour
    get :call, :address => 'foo', :not_before => time, :channel => channel.name
    assert_equal time.to_i, QueuedCall.first.not_before.time.to_i
  end
end
