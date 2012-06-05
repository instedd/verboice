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

describe ApiController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make project: project }
  let(:channel) { account.channels.make :call_flow => call_flow, :account => account }
  let(:schedule) { project.schedules.make }

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

    it "schedule call in specific schedule" do
      get :call, :address => 'foo', :channel => channel.name, :schedule => schedule.name
      QueuedCall.first.schedule.should == schedule
    end
  end

  it "call state" do
    project = Project.make account: @controller.current_account
    call_log = CallLog.make :call_flow => CallFlow.make(project: project)
    get :call_state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    result['call_id'].should == call_log.id
    result['state'].should == call_log.state.to_s
  end
end
