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

describe ResourcesController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    @project = @account.projects.make
    @other_project = Project.make
    sign_in @account
  end

  let!(:resource1) { Resource.make :project => @project, :name => 'foo' }
  let!(:resource2) { Resource.make :project => @project, :name => 'bar' }
  let!(:other_resource) { Resource.make :project => @other_project }

  describe "GET index" do
    it "assigns all project resources as @resources" do
      get :index, :project_id => @project.id, :format => :json
      controller.resources.should eq([resource1, resource2])
    end

    it "searches project resources" do
      get :index, :project_id => @project.id, :q => 'foo', :format => :json
      controller.resources.should eq([resource1])
    end

  end

  describe "GET show" do
    it "assigns resource as @resource" do
      get :show, :project_id => @project.id, :id => resource1.to_param, :format => :json
      controller.resource.should eq(resource1)
    end

    it "fails if the requested resource is not in current project" do
      expect {
        get :show, {:project_id => @project.id, :id => other_resource.to_param}
      }.should raise_error
    end
  end

  describe "GET find" do
    it "finds resources by guid" do
      get :find, :project_id => @project.id, :guid => resource1.guid, :format => :json
      response.body.should eq(resource1.to_json(:include => :localized_resources))
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new Resource" do
        expect {
          post :create, {:project_id => @project.id, :resource => Resource.plan}
        }.to change(Resource, :count).by(1)
      end

      it "assigns a newly created resource as @resource" do
          post :create, {:project_id => @project.id, :resource => Resource.plan}
          controller.resource.should be_a(Resource)
          controller.resource.should be_persisted
        end

      it "renders the newly created resources as json" do
        post :create, :project_id => @project.id, :resource => {:name => 'foo'}, :format => :json
        response.body.should eq(Resource.order(:id).last.to_json(:include => :localized_resources))
      end

      it "assigns the current project to the resource" do
        post :create, {:project_id => @project.id, :resource => Resource.plan}
        controller.resource.project.should eq(@project)
      end

      it "returns the correct amount of nested localized_resources" do
        # This was returning duplicated localized resources for some reason. It seems to be a decent_exposure issue with nested attributes
        resource_json = {"name"=>"Say hello",
          "localized_resources_attributes"=>
            {"0"=>{"language"=>"en", "type"=>"TextLocalizedResource", "text"=>"Hello"},
            "1"=>{"language"=>"es", "type"=>"TextLocalizedResource"}}}
        post :create, :project_id => @project.id, :resource => resource_json, :format => :json
        JSON.parse(response.body)["localized_resources"].count.should eq(2)
      end
    end

    describe "with invalid params" do
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested resource" do
        put :update, {:project_id => @project.id, :id => resource1.to_param, :resource => {:name => 'new name'}}
        resource1.reload.name.should eq('new name')
      end

      it "assigns the requested resource as @resource" do
        put :update, {:id => resource1.to_param, :resource => Resource.plan, :project_id => @project.to_param}
        controller.resource.should eq(resource1)
      end

      it "renders the requested resources as json" do
        put :update, {:project_id => @project.id, :id => resource1.to_param, :resource => {}, :format => :json}
        response.body.should eq(resource1.to_json(:include => :localized_resources))
      end

      it "returns the correct amount of nested localized_resources" do
        localized_res1 = TextLocalizedResource.make resource: resource1, language: "en"
        localized_res2 = TextLocalizedResource.make resource: resource1, language: "es"
        # This was returning duplicated localized resources for some reason. It seems to be a decent_exposure issue with nested attributes
        resource_json =  {"name"=>"new name",
          "localized_resources_attributes"=>
          {"0"=>
            {"language"=>"en",
             "type"=>"UploadLocalizedResource",
             "filename"=>"05 Ipanema.mp3"},
           "1"=>{"id"=>localized_res2.id, "language"=>"es", "type"=>"TextLocalizedResource"}}}
        put :update, {:id => resource1.to_param, :project_id => @project.id, :resource => resource_json, :format => :json}
        JSON.parse(response.body)["localized_resources"].count.should eq(2)
      end

    end

    describe "with invalid params" do
    end
  end

end
