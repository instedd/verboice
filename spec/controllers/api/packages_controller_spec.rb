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
require 'json-schema'

describe Api::FlowResults::PackagesController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }

  before(:each) do
    sign_in account
  end

  def assert_json_api_compliance(json)
    schema = File.join(Rails.root, 'spec/fixtures/json_api_schema.json')
    JSON::Validator.validate!(schema, json)
  end

  def assert_json_api_not_found_error(response)
    expect(response).to be_not_found
    json = JSON.parse response.body
    assert_json_api_compliance(json)
  end

  describe("list data packages for a call flow") do
    describe("happy path") do
      let!(:call_flow) { project.call_flows.make :name => "Flow", :mode => :flow }

      it "gets data packages for a call flow" do
        get :index, project_id: project.id, call_flow_id: call_flow.id

        expect(response).to be_ok

        json = JSON.parse response.body

        assert_json_api_compliance(json)

        expect(json["data"].length).to eq(1)

        package = json["data"][0]
        expect(package["type"]).to eq("packages")
        expect(package["id"]).to eq(call_flow.current_data_package.uuid)

        expect(json["links"]["self"]).to eq(api_project_call_flow_flow_results_packages_url(project.id, call_flow.id))
      end
    end

    describe("error modes") do
      it "returns 404 when callflow does not belong to project" do
        call_flow = CallFlow.make :name => "Flow", :mode => :flow
        get :index, project_id: project.id, call_flow_id: call_flow.id
        assert_json_api_not_found_error(response)
      end

      it "returns 404 when callflow works in callback mode" do
        call_flow = project.call_flows.make :name => "Flow", :mode => "callback_url"
        get :index, project_id: project.id, call_flow_id: call_flow.id
        assert_json_api_not_found_error(response)
      end

      it "returns 404 when project does not exist" do
        call_flow = CallFlow.make :name => "Flow", :mode => :flow
        get :index, project_id: project.id * 100, call_flow_id: call_flow.id
        assert_json_api_not_found_error(response)
      end

      it "returns 404 when call flow does not exist" do
        get :index, project_id: project.id, call_flow_id: 42
        assert_json_api_not_found_error(response)
      end

      it "returns 404 when user cannot read project" do
        project2 = Project.make

        expect(account.id).not_to eq(project2.account.id)

        call_flow = project2.call_flows.make :name => "Flow", :mode => :flow
        get :index, project_id: project2.id, call_flow_id: call_flow.id
        assert_json_api_not_found_error(response)
      end
    end
  end

  describe("descriptor") do
    it "returns a valid descriptor wrapped in a valid jsonapi envelope" do
      call_flow = project.call_flows.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        {
          'id' => 1,
          'root' => 1,
          'type' => 'play',
          'name' => 'Play number one',
          'resource' => {
            "guid" => "foo"
          }
        }
      ]
      call_flow.save!

      get :show, project_id: project.id, call_flow_id: call_flow.id, id: call_flow.current_data_package.uuid

      expect(response).to be_ok

      json = JSON.parse response.body
      assert_json_api_compliance(json)

      descriptor = json["data"]["attributes"]
      schema = File.join(Rails.root, 'spec/fixtures/data_package_schema.json')
      JSON::Validator.validate!(schema, descriptor.to_json)

      expected_relationship_link = responses_api_project_call_flow_flow_results_package_url(project.id, call_flow.id, call_flow.current_data_package.uuid)
      expect(json["data"]["relationships"]["responses"]["links"]["related"]).to eq(expected_relationship_link)
    end

    it "returns 404 when requested package is not current call flow package" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      get :show, project_id: project.id, call_flow_id: call_flow.id, id: "foo"
      assert_json_api_not_found_error(response)
    end

    it "returns 404 when user cannot read project" do
      project2 = Project.make

      expect(account.id).not_to eq(project2.account.id)

      call_flow = project2.call_flows.make :name => "Flow", :mode => :flow
      get :show, project_id: project2.id, call_flow_id: call_flow.id, id: call_flow.current_data_package.uuid
      assert_json_api_not_found_error(response)
    end
  end

  describe("responses") do
    it "has a happy path" do
      call_flow = project.call_flows.make :name => "Flow", :mode => :flow

      get :responses, project_id: project.id, call_flow_id: call_flow.id, id: call_flow.current_data_package.uuid

      expect(response).to be_ok

      json = JSON.parse response.body
      assert_json_api_compliance(json)

      data = json["data"]
      expect(data["type"]).to eq("flow-results-data")
      expect(data["id"]).to eq(call_flow.current_data_package.uuid)
      expect(data["attributes"]["responses"]).to eq([])
    end
  end
end
