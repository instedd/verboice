require 'test_helper'

class ApiChannelsControllerTest < ActionController::TestCase
  include Devise::TestHelpers

  def setup
    @account = Account.make
    sign_in @account
  end

  test "create sip2sip channel" do
    app = @account.applications.make

    data = {kind: "sip2zip", name: "foo", application: app.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    channels = @account.channels.all
    assert_equal 1, channels.length
    assert_equal @account, channels[0].account
    assert_equal app.id, channels[0].application_id
    assert_equal data[:name], channels[0].name
    assert_equal data[:kind], channels[0].kind
    assert_equal data[:username], channels[0].username
    assert_equal data[:password], channels[0].password
  end

  test "create sip2sip channel errors" do
    app = @account.applications.make

    data = {kind: "sip2zip", application: app.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    assert_equal 0, @account.channels.count

    response = JSON.parse(@response.body).with_indifferent_access
    assert_equal "There were problems creating the channel", response[:summary]
    assert_equal ["name" => "can't be blank"], response[:properties]
  end

  test "delete channel" do
    BrokerClient.expects(:delete_channel)

    app = @account.applications.make
    chan = @account.channels.make :application => app, :name => 'foo'

    delete :destroy, :name => chan.name
    assert_response :ok

    assert_equal 0, @account.channels.count
  end
end
