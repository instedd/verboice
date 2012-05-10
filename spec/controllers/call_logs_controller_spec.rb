require 'spec_helper'

describe CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:application) {Application.make :account => account}
  let(:channel) { account.channels.make :application => application, :account => account}
  let(:queue) { account.call_queues.make :weekdays => "1" }

  before(:each) do
    BrokerClient.stub(:notify_call_queued)
    sign_in account
  end

  it 'should get queued calls' do
    calls = 10.times.map { QueuedCall.make :channel => channel }
    get :queued
    response.should be_success
    assigns(:calls).should eq(calls.sort_by(&:id).reverse)
  end

  it 'should enqueue a call' do
    expect {
      post :enqueue, :addresses => "1", :channel_id => channel.id, :queue_id => queue.id
    }.to change(QueuedCall, :count).by(1)
    response.should be_redirect
  end

  it 'should enqueue a call not before specific date' do
    not_before = DateTime.new(2012, 1, 1, 16, 0, 0)
    BrokerClient.should_receive(:notify_call_queued).with(channel.id,not_before + 1)

    expect {
      post :enqueue, :addresses => "1", :channel_id => channel.id, :queue_id => queue.id, :not_before => not_before
    }.to change(QueuedCall, :count).by(1)

    enqueued_call = QueuedCall.last
    enqueued_call.call_queue_id.should eq(queue.id)
    enqueued_call.application_id.should eq(application.id)
    enqueued_call.not_before.should eq(not_before + 1)

    response.should be_redirect
  end

  it 'should enqueue multiple calls' do
    expect {
      post :enqueue, :addresses => "0\n1\n2", :channel_id => channel.id, :queue_id => queue.id
    }.to change(QueuedCall, :count).by(3)
    response.should be_redirect

    actual = QueuedCall.all
    [0,1,2].each do |num|
      actual[num].address.should eq(num.to_s)
    end
  end

end
