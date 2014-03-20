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

describe ExternalServicesController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:external_service) { ExternalService.make project: project }
  let!(:other_external_service) { ExternalService.make }
  let(:errors) { double('errors', :full_messages => [], :empty? => false, :any? => false, :[] => {}) }

  describe "GET index" do
    it "assigns all project external_services as external_services" do
      get :index, {:project_id => project.to_param}
      controller.external_services.should eq([external_service])
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new ExternalService" do
        expect {
          post :create, {:external_service => ExternalService.plan, :project_id => project.to_param}
        }.to change(ExternalService, :count).by(1)
      end

      it "assigns a newly created external_service as external_service" do
        post :create, {:external_service => ExternalService.plan, :project_id => project.to_param}
        controller.external_service.should be_a(ExternalService)
        controller.external_service.should be_persisted
      end

      it "assigns the project to the created external_service" do
        post :create, {:external_service => ExternalService.plan, :project_id => project.to_param}
        controller.external_service.project.should eq(project)
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved external_service as @external_service" do
        ExternalService.any_instance.stub(:save).and_return(false)
        post :create, {:external_service => {}, :project_id => project.to_param}
        controller.external_service.should be_a_new(ExternalService)
      end

      it "re-renders the 'index' template" do
        ExternalService.any_instance.stub(:save).and_return(false)
        ExternalService.any_instance.stub(:errors).and_return(errors)
        post :create, {:external_service => {}, :project_id => project.to_param}
        response.should render_template("index")
      end
    end

    it "fails if the requested external_service is not in current account projects" do
      post :create, {:project_id => other_external_service.project.to_param, :external_service => ExternalService.plan}
      response.status.should eq(404)
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested external_service" do
        put :update, {:id => external_service.to_param, :external_service => {:name => 'new name'}, :project_id => project.to_param}
        external_service.reload.name.should eq('new name')
      end

      it "assigns the requested external_service as @external_service" do
        put :update, {:id => external_service.to_param, :external_service => ExternalService.plan, :project_id => project.to_param}
        controller.external_service.should eq(external_service)
      end
    end

    describe "with invalid params" do
      it "assigns the external_service as @external_service" do
        ExternalService.any_instance.stub(:save).and_return(false)
        put :update, {:id => external_service.to_param, :external_service => {}, :project_id => project.to_param}
        controller.external_service.should eq(external_service)
      end

      it "re-renders the 'index' template" do
        ExternalService.any_instance.stub(:save).and_return(false)
        ExternalService.any_instance.stub(:errors).and_return(errors)
        put :update, {:id => external_service.to_param, :external_service => {}, :project_id => project.to_param}
        response.should render_template("index")
      end
    end

    it "fails if the requested external_service is not in current account projects" do
      put :update, {:id => other_external_service.to_param, :external_service => ExternalService.plan, :project_id => other_external_service.project.to_param}
      response.status.should eq(404)
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested external_service" do
      expect {
        delete :destroy, {:id => external_service.to_param, :project_id => project.to_param}
      }.to change(ExternalService, :count).by(-1)
    end

    it "fails if the requested external_service is not in current account projects" do
      delete :update, {:id => other_external_service.to_param, :external_service => ExternalService.plan, :project_id => other_external_service.project.to_param}
      response.status.should eq(404)
    end

    it "cleans external_service call flows before destroy" do
      ExternalService.any_instance.should_receive(:clean_call_flows)
      delete :destroy, {:id => external_service.to_param, :project_id => project.to_param}
    end
  end

  describe "PUT update_manifest" do
    before(:each) do
      ExternalService.any_instance.stub(:update_manifest!)
    end

    it "updates the manifest of the requested external_service" do
      ExternalService.any_instance.should_receive(:update_manifest!)
      put :update_manifest, {:id => external_service.to_param, :project_id => project.to_param}
    end


     it "fails if the requested external_service is not in current account projects" do
      put :update_manifest, {:id => other_external_service.to_param, :project_id => other_external_service.project.to_param}
      response.status.should eq(404)
    end
  end
end
