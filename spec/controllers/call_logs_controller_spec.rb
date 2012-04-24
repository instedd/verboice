require 'spec_helper'

describe CallLogsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  let(:channel) do
    Channel.make :account => @account
  end

  let(:queue) do
    CallQueue.make :account => @account
  end

  it 'should get queued calls' do
    calls = 10.times.map { QueuedCall.make :channel => channel }
    get :queued
    response.should be_success
    assigns(:calls).should eq(calls.sort_by(&:id).reverse)
  end

if CallLogsController::ENQUEUE_CALLS_ENABLED
  it 'should enqueue a call' do
    expect {
      post :enqueue, :addresses => "1", :channel_id => channel.id, :queue_id => queue.id
    }.to change(QueuedCall, :count).by(1)
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

end
