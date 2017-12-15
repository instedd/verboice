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

describe Api::FlowResults::PackagesController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }

  before(:each) do
    sign_in account
  end

  describe("happy path") do
    let!(:call_flow) { project.call_flows.make :name => "Flow", :mode => :flow }

    it "gets data packages for a call flow" do
      get :index, project_id: project.id, call_flow_id: call_flow.id

      response.should be_ok

      json = JSON.parse response.body
      json.length.should eq(1)
      json[0].should eq(api_project_call_flow_flow_results_package_url(project.id, call_flow.id, call_flow.current_data_package.uuid))
    end
  end

  describe("error modes") do
    it "returns 404 when callflow does not belong to project" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      get :index, project_id: project.id, call_flow_id: call_flow.id
      response.should be_not_found
    end

    it "returns empty array when callflow has works in callback mode" do
      call_flow = project.call_flows.make :name => "Flow", :mode => "callback_url"
      get :index, project_id: project.id, call_flow_id: call_flow.id
      response.should be_ok

      json = JSON.parse response.body
      json.length.should eq(0)
    end
  end
end
