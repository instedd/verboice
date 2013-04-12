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
      response.body.should eq(chan.to_json)
    end
  end

  describe "create" do
    it "create custom channel" do
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
      channels.length.should == 1
      channels[0].account.should == @account
      channels[0].call_flow_id.should == call_flow.id
      channels[0].name.should == data[:name]
      channels[0].username.should == data[:config][:username]
      channels[0].password.should == data[:config][:password]
      channels[0].number.should == data[:config][:number]
      channels[0].limit.should == data[:config][:limit]
      channels[0].domain.should == data[:config][:domain]
      channels[0].direction.should == data[:config][:direction]
      channels[0].register.should == data[:config][:register]
      channels[0].class.should == Channels::CustomSip
    end

    it "create custom channel errors" do
      data = {kind: "custom", call_flow: call_flow.name}
      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, format: :json
      assert_response :ok

      @account.channels.count.should == 0

      response = JSON.parse(@response.body).with_indifferent_access
      response[:summary].should == "There were problems creating the Channel"
      response[:properties].should == ["name" => "can't be blank"]
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
      chan.name.should eq('updated name')
      chan.username.should eq('updated username')
      chan.password.should eq('updated secret')
    end

    it "should tell erros" do
      chan = Channels::Custom.make account: @account, call_flow: call_flow, name: 'the_channel'

      data = {:name => ''}
      @request.env['RAW_POST_DATA'] = data.to_json
      put :update, name: chan.name, format: :json

      assert_response :ok

      response = JSON.parse(@response.body).with_indifferent_access
      response[:summary].should == "There were problems updating the Channel"
      response[:properties].should == ["name" => "can't be blank"]

      chan.reload.name.should eq('the_channel')
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

      @account.channels.count.should == 0
    end
  end
end
