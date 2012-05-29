require 'spec_helper'

describe CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) {Project.make :account => account}
  let(:call_flow) { CallFlow.make :project => project }
  let(:channel) { account.channels.make :call_flow => call_flow, :account => account}

  before(:each) do
    sign_in account
  end

  it 'should get queued calls' do
    calls = 10.times.map { QueuedCall.make :channel => channel }
    get :queued
    response.should be_success
    assigns(:calls).should eq(calls.sort_by(&:id).reverse)
  end
end
