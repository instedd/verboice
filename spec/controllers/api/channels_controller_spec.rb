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
  before(:each) do
    @account = Account.make
    sign_in @account
  end

  let(:project) { @account.projects.make }
  let(:call_flow) { CallFlow.make project: project }

  describe "get" do
    it "should return not found for non existing channel" do
      get :get, :name => 'non_existing'
      assert_response :not_found
    end

    it "should return channel" do
      chan = Channels::CustomSip.make account: @account, call_flow: call_flow

      get :get, :name => chan.name

      assert_response :ok
      expect(response.body).to eq(chan.to_json)
    end

    it "should return not found for non existing channel by id" do
      get :get_by_id, :id => 0
      assert_response :not_found
    end

    it "should return from id" do
      chan = Channels::CustomSip.make account: @account, call_flow: call_flow

      get :get_by_id, :id => chan.id

      assert_response :ok
      expect(response.body).to eq(chan.to_json)
    end

    it "should return shared channel from id" do
      other_account = Account.make
      other_call_flow = other_account.projects.make.call_flows.make
      shared_read_chan = Channels::CustomSip.make account: other_account, call_flow: other_call_flow
      ChannelPermission.create! model_id: shared_read_chan.id, account_id: @account.id, role: "read"

      get :get_by_id, :id => shared_read_chan.id

      assert_response :ok
      expect(response.body).to eq(shared_read_chan.to_json(account_id: @account.id))
    end

    it "get should include status when channel status is computed by BrokerClient" do
      chan = Channels::CustomSip.make account: @account, call_flow: call_flow
      allow(BrokerClient).to receive(:channel_status).and_return({
        "some_id" => {:messages => ["not found"], :ok => false},
        "other_id" => {:messages => nil, :ok => true},
        chan.id => {:messages => ["connection error"], :ok => false}
      })
      get :get, :name => chan.name

      response = JSON.parse(@response.body).with_indifferent_access
      expect(response["status"]["messages"]).to eq(["connection error"])
      expect(response["status"]["ok"]).to eq(false)
    end

    it "get_by_id should include status when channel status is computed by BrokerClient" do
      chan = Channels::CustomSip.make account: @account, call_flow: call_flow
      allow(BrokerClient).to receive(:channel_status).and_return({
        18 => {:messages => ["NOT FOUND"], :ok => false},
        21 => {:messages => nil, :ok => true},
        chan.id => {:messages => ["NOT FOUND"], :ok => false}
      })
      get :get_by_id, :id => chan.id

      response = JSON.parse(@response.body).with_indifferent_access
      expect(response["status"]["messages"]).to eq(["NOT FOUND"])
      expect(response["status"]["ok"]).to eq(false)
    end
  end

  describe "list" do
    it "should return all channels" do
      chan1 = Channels::CustomSip.make account: @account, call_flow: call_flow
      chan2 = Channels::CustomSip.make account: @account, call_flow: call_flow

      get :list

      assert_response :ok
      expect(response.body).to eq([chan1.name, chan2.name].to_json)
    end
  end

  describe "all" do
    it "should return all channels" do
      chan1 = Channels::CustomSip.make account: @account, call_flow: call_flow
      chan2 = Channels::CustomSip.make account: @account, call_flow: call_flow

      get :all

      assert_response :ok
      expect(response.body).to eq([chan1, chan2].to_json)
    end

    it "should include shared channels" do
      other_account = Account.make
      other_call_flow = other_account.projects.make.call_flows.make

      chan = Channels::CustomSip.make account: @account, call_flow: call_flow
      shared_read_chan = Channels::CustomSip.make account: other_account, call_flow: other_call_flow
      shared_admin_chan = Channels::CustomSip.make account: other_account, call_flow: other_call_flow

      ChannelPermission.create! model_id: shared_read_chan.id, account_id: @account.id, role: "read"
      ChannelPermission.create! model_id: shared_admin_chan.id, account_id: @account.id, role: "admin"

      get :all

      assert_response :ok
      expect(response.body).to eq([chan, shared_read_chan, shared_admin_chan].to_json(account_id: @account.id))
    end

    it "should include status for channels with their status computed by BrokerClient" do
      chan1 = Channels::CustomSip.make account: @account, call_flow: call_flow
      chan2 = Channels::CustomSip.make account: @account, call_flow: call_flow
      allow(BrokerClient).to receive(:channel_status).and_return({
        chan1.id => {:messages => nil, :ok => true},
      })

      get :all

      response = JSON.parse(@response.body)
      expect(response[0]["status"]).to eq({"messages" => nil, "ok" => true})
      expect(response[1]["status"]).to be_nil
    end
  end

  describe "create" do
    it "create custom channel" do
      data = {kind: "custom", name: "foo", call_flow: call_flow.name}
      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, format: :json
      assert_response :ok

      channels = @account.channels.all
      expect(channels.length).to eq(1)
      expect(channels[0].account).to eq(@account)
      expect(channels[0].call_flow_id).to eq(call_flow.id)
      expect(channels[0].name).to eq(data[:name])
      expect(channels[0].class).to eq(Channels::Custom)
    end

    it "create a custom sip channel" do
      data = {
        kind: 'sip',
        name: 'foo',
        call_flow: call_flow.name,
        config: {
          username: 'john',
          password: 'secret',
          number: '123',
          limit: '3',
          domain: 'foobar.com',
          direction: 'both',
          register: true
        }
      }

      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, format: :json

      assert_response :ok

      channels = @account.channels.all
      expect(channels.length).to eq(1)
      expect(channels[0].account).to eq(@account)
      expect(channels[0].call_flow_id).to eq(call_flow.id)
      expect(channels[0].name).to eq(data[:name])
      expect(channels[0].username).to eq(data[:config][:username])
      expect(channels[0].password).to eq(data[:config][:password])
      expect(channels[0].number).to eq(data[:config][:number])
      expect(channels[0].limit).to eq(data[:config][:limit])
      expect(channels[0].domain).to eq(data[:config][:domain])
      expect(channels[0].direction).to eq(data[:config][:direction])
      expect(channels[0].register).to eq(data[:config][:register])
      expect(channels[0].class).to eq(Channels::CustomSip)
    end

    it "create custom channel errors" do
      data = {kind: "custom", call_flow: call_flow.name}
      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, format: :json
      assert_response :ok

      expect(@account.channels.count).to eq(0)

      response = JSON.parse(@response.body).with_indifferent_access
      expect(response[:summary]).to eq("There were problems creating the Channel")
      expect(response[:properties]).to eq(["name" => "can't be blank"])
    end
  end

  describe "update" do
    it "should return not found for non existing channel" do
      put :update, :name => 'non_existing'
      assert_response :not_found
    end

    it "should update channel" do
      chan = Channels::CustomSip.make account: @account, call_flow: call_flow

      data = {
        name: 'updated name',
        config: {
          username: 'updated username',
          password: 'updated secret'
        }
      }
      @request.env['RAW_POST_DATA'] = data.to_json
      put :update, name: chan.name, format: :json
      assert_response :ok

      chan = chan.reload
      expect(chan.name).to eq('updated name')
      expect(chan.username).to eq('updated username')
      expect(chan.password).to eq('updated secret')
    end

    it "should tell erros" do
      chan = Channels::Custom.make account: @account, call_flow: call_flow, name: 'the_channel'

      data = {:name => ''}
      @request.env['RAW_POST_DATA'] = data.to_json
      put :update, name: chan.name, format: :json

      assert_response :ok

      response = JSON.parse(@response.body).with_indifferent_access
      expect(response[:summary]).to eq("There were problems updating the Channel")
      expect(response[:properties]).to eq(["name" => "can't be blank"])

      expect(chan.reload.name).to eq('the_channel')
    end
  end

  describe "delete" do
    it "should return not found for non existing channel" do
      delete :destroy, :name => 'non_existing'
      assert_response :not_found
    end

    it "delete channel" do
      chan = Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :name => 'foo', :account => @account

      delete :destroy, :name => chan.name
      assert_response :ok

      expect(@account.channels.count).to eq(0)
    end
  end

  describe "enable" do
    it "should return not found for non existing channel" do
      post :enable, :id => 1
      assert_response :not_found
    end

    it "enables a channel" do
      chan = Channel.all_leaf_subclasses.sample.make :name => 'foo', :account => @account, :enabled => false

      expect(chan).not_to be_enabled

      post :enable, :id => chan.id
      assert_response :ok

      expect(chan.reload).to be_enabled
    end
  end

  describe "disable" do
    it "should return not found for non existing channel" do
      post :enable, :id => 1
      assert_response :not_found
    end

    it "disables a channel" do
      chan = Channel.all_leaf_subclasses.sample.make :name => 'foo', :account => @account

      expect(chan).to be_enabled

      post :disable, :id => chan.id
      assert_response :ok

      expect(chan.reload).not_to be_enabled
    end
  end
end
