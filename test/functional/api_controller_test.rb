require 'test_helper'

class ApiControllerTest < ActionController::TestCase
  include Devise::TestHelpers

  def setup
    account = Account.make
    sign_in account
  end

  test "call" do
    call_log = CallLog.make
    @controller.current_account.expects(:call).returns(call_log)
    get :call, :address => 'foo', :callback => 'bar'
    result = JSON.parse(@response.body)
    assert_equal call_log.id, result['call_id']
  end

end
