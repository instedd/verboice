require 'spec_helper'

describe ApiController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:application) {Application.make :account => account}
  let(:channel) { account.channels.make :application => application, :account => account}
  let(:queue) { account.call_queues.make }

  before(:each) do
    sign_in account
  end

  context "call" do
    let(:broker_client) { double('broker_client') }

    before(:each) do
      BrokerClient.stub(:new).and_return(broker_client)
      broker_client.stub(:notify_call_queued)
    end

    it "calls" do
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar'
      call_log = CallLog.last
      result = JSON.parse(@response.body)
      result['call_id'].should == call_log.id
    end

    it "schedule call in the future" do
      time = Time.now.utc + 1.hour
      get :call, :address => 'foo', :not_before => time, :channel => channel.name
      QueuedCall.first.not_before.time.to_i.should == time.to_i
    end

    it "schedule call in specific queue" do
      get :call, :address => 'foo', :channel => channel.name, :queue => queue.name
      QueuedCall.first.call_queue.should == queue
    end
  end

  it "call state" do
    call_log = CallLog.make :application => Application.make(:account => @controller.current_account)
    get :call_state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    result['call_id'].should == call_log.id
    result['state'].should == call_log.state.to_s
  end
end
