require 'test_helper'

class ApiControllerTest < ActionController::TestCase
  include Devise::TestHelpers

  def setup
    @app = Application.make
    sign_in @app.account
  end

  test "call application" do
    @controller.current_account.applications.expects(:find).with(@app.id).returns(@app)
    @app.expects(:call).with('foo').returns(CallLog.make)

    get :call, :address => 'foo', :application => @app.id
  end

  test "call with callback creates new application" do
    Application.expects(:create!).with(
      :name => 'http://foo.com',
      :callback_url => 'http://foo.com',
      :account => @controller.current_account).returns(@app)
    call_log = CallLog.make
    @app.expects(:call).with('foo').returns(call_log)

    get :call, :address => 'foo', :callback => 'http://foo.com'
    result = JSON.parse(@response.body)
    assert_equal call_log.id, result['call_id']
  end

end