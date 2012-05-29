require 'spec_helper'

describe ApiChannelsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  it "create custom channel" do
    project = @account.projects.make
    call_flow = CallFlow.make project: project
    project.default_call_flow = call_flow

    data = {kind: "custom", name: "foo", call_flow: call_flow.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    channels = @account.channels.all
    channels.length.should == 1
    channels[0].account.should == @account
    channels[0].call_flow_id.should == call_flow.id
    channels[0].name.should == data[:name]
    channels[0].kind.should == data[:kind]
    channels[0].username.should == data[:username]
    channels[0].password.should == data[:password]
  end

  it "create custom channel errors" do
    project = @account.projects.make
    call_flow = CallFlow.make project: project

    data = {kind: "custom", call_flow: call_flow.name, username: 'xyz', password: 'pass'}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    @account.channels.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the channel"
    response[:properties].should == ["name" => "can't be blank"]
  end

  it "delete channel" do
    broker_client = double('broker_client')
    BrokerClient.stub(:new).and_return(broker_client)
    broker_client.should_receive(:delete_channel)

    project = @account.projects.make
    call_flow = CallFlow.make project: project
    chan = @account.channels.make :call_flow => call_flow, :name => 'foo'

    delete :destroy, :name => chan.name
    assert_response :ok

    @account.channels.count.should == 0
  end
end
