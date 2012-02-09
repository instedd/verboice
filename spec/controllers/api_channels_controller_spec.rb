require 'spec_helper'

describe ApiChannelsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  it "create sip2sip channel" do
    app = @account.applications.make

    data = {kind: "sip2zip", name: "foo", application: app.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    channels = @account.channels.all
    channels.length.should == 1
    channels[0].account.should == @account
    channels[0].application_id.should == app.id
    channels[0].name.should == data[:name]
    channels[0].kind.should == data[:kind]
    channels[0].username.should == data[:username]
    channels[0].password.should == data[:password]
  end

  it "create sip2sip channel errors" do
    app = @account.applications.make

    data = {kind: "sip2zip", application: app.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    @account.channels.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the channel"
    response[:properties].should == ["name" => "can't be blank"]
  end

  it "delete channel" do
    BrokerClient.should_receive(:delete_channel)

    app = @account.applications.make
    chan = @account.channels.make :application => app, :name => 'foo'

    delete :destroy, :name => chan.name
    assert_response :ok

    @account.channels.count.should == 0
  end
end
