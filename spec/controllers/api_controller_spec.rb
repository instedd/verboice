require 'spec_helper'

describe ApiController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  it "call" do
    call_log = CallLog.make
    @controller.current_account.should_receive(:call).and_return(call_log)
    get :call, :address => 'foo', :callback => 'bar'
    result = JSON.parse(@response.body)
    result['call_id'].should == call_log.id
  end

  it "call state" do
    call_log = CallLog.make :application => Application.make(:account => @controller.current_account)
    get :call_state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    result['call_id'].should == call_log.id
    result['state'].should == call_log.state.to_s
  end

  it "schedule call in the future" do
    channel = @account.channels.make
    BrokerClient.should_receive(:notify_call_queued).with(channel.id)
    time = Time.now.utc + 1.hour
    get :call, :address => 'foo', :not_before => time, :channel => channel.name
    QueuedCall.first.not_before.time.to_i.should == time.to_i
  end
end
