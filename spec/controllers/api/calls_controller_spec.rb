# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe Api::CallsController do
  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make project: project }
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }
  let(:schedule) { project.schedules.make }

  before(:each) do
    sign_in account
  end

  context "call" do
    before(:each) do
      allow(BrokerClient).to receive(:notify_call_queued)
    end

    it "calls" do
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar'
      call_log = CallLog.last
      result = JSON.parse(@response.body)
      expect(result['call_id']).to eq(call_log.id)
      expect(call_log.channel_id).to eq(channel.id)
    end

    it "calls with channel id" do
      get :call, :address => 'foo', :channel_id => channel.id, :callback => 'bar'
      call_log = CallLog.last
      result = JSON.parse(@response.body)
      expect(result['call_id']).to eq(call_log.id)
      expect(call_log.channel_id).to eq(channel.id)
    end

    it "schedule call in the future" do
      time = Time.now.utc + 1.hour
      get :call, :address => 'foo', :not_before => time, :channel => channel.name
      expect(QueuedCall.first.not_before.time.to_i).to eq(time.to_i)
    end

    it "schedule call with deadline" do
      time = Time.now.utc + 1.hour
      get :call, :address => 'foo', :not_after => time, :channel => channel.name
      expect(QueuedCall.first.not_after.time.to_i).to eq(time.to_i)
    end

    it "doesn't schedule overdue call" do
      time = Time.now.utc - 1.hour
      get :call, :address => 'foo', :not_after => time, :channel => channel.name
      expect(QueuedCall.count).to be(0)
    end

    it "schedule call in specific schedule" do
      get :call, :address => 'foo', :channel => channel.name, :schedule => schedule.name
      expect(QueuedCall.first.schedule).to eq(schedule)
    end

    it "calls with call flow id" do
      call_flow_2 = CallFlow.make project: project
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar', :call_flow_id => call_flow_2.id
      expect(CallLog.last.call_flow).to eq(call_flow_2)
    end

    it "calls with call flow name" do
      call_flow_2 = CallFlow.make project: project
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar', :call_flow => call_flow_2.name
      expect(CallLog.last.call_flow).to eq(call_flow_2)
    end

    it "rejects a call without a channel" do
      get :call
      expect(response).not_to be_success
    end

    it "rejects a call with empty address" do
      get :call, :channel => channel.name
      expect(response).not_to be_success
    end

    it "rejects a call using a disabled channel" do
      channel.disable!
      get :call, :address => 'foo', :channel_id => channel.id, :callback => 'bar'
      expect(response).not_to be_success
    end
  end

  it "call state" do
    project = Project.make account: @controller.current_account
    call_log = CallLog.make :call_flow => CallFlow.make(project: project)
    get :state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    expect(result['call_id']).to eq(call_log.id)
    expect(result['state']).to eq(call_log.state.to_s)
  end

  it "call state for an unknown call" do
    get :state, :id => 12345678
    assert_response :not_found
  end

  it "cancells a call" do
    project = Project.make account: @controller.current_account
    call_log = CallLog.make :call_flow => CallFlow.make(project: project)
    queued_call = QueuedCall.make :call_log => call_log

    post :cancel, :id => call_log.id

    result = JSON.parse(@response.body)
    expect(result['call_id']).to eq(call_log.id)
    expect(result['state']).to eq('canceled')

    call_log = CallLog.find_by_id(call_log.id)
    expect(call_log.state).to eq(:canceled)

    expect(QueuedCall.find_by_id(queued_call.id)).to be_nil
  end
end
