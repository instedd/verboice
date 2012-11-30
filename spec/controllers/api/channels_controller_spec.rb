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

describe Api::ChannelsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  it "create custom channel" do
    project = @account.projects.make
    call_flow = CallFlow.make project: project
    project.default_call_flow = call_flow

    data = {kind: "custom", name: "foo", call_flow: call_flow.name}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    channels = @account.channels.all
    channels.length.should == 1
    channels[0].account.should == @account
    channels[0].call_flow_id.should == call_flow.id
    channels[0].name.should == data[:name]
    channels[0].class.should == Channels::Custom
  end

  it "create custom channel errors" do
    project = @account.projects.make
    call_flow = CallFlow.make project: project

    data = {kind: "custom", call_flow: call_flow.name}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, format: :json
    assert_response :ok

    @account.channels.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the Channel"
    response[:properties].should == ["name" => "can't be blank"]
  end

  it "delete channel" do
    broker_client = double('broker_client')
    BrokerClient.stub(:new).and_return(broker_client)
    broker_client.should_receive(:delete_channel)

    project = @account.projects.make
    call_flow = CallFlow.make project: project

    chan = Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :name => 'foo', :account => @account

    delete :destroy, :name => chan.name
    assert_response :ok

    @account.channels.count.should == 0
  end
end
