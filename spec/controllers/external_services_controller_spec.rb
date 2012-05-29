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

  describe "GET show" do
    it "assigns the requested external_service as external_service" do
      get :show, {:project_id => project.to_param, :id => external_service.to_param}
      controller.external_service.should eq(external_service)
    end

    it "fails if the requested external_service is not in current account projects" do
      expect {
        get :show, {:project_id => other_external_service.project.to_param, :id => other_external_service.to_param}
      }.should raise_error
    end
  end

  describe "GET new" do
    it "assigns a new external_service as external_service" do
      get :new, {:project_id => project.to_param}
      controller.external_service.should be_a_new(ExternalService)
    end

    it "assigns the project to the new external_service" do
      get :new, {:project_id => project.to_param}
      controller.external_service.project.should eq(project)
    end
  end

  describe "GET edit" do
    it "assigns the requested external_service as @external_service" do
      get :edit, {:project_id => project.to_param, :id => external_service.to_param}
      controller.external_service.should eq(external_service)
    end

    it "fails if the requested external_service is not in current account projects" do
      expect {
        get :edit, {:project_id => other_external_service.project.to_param, :id => other_external_service.to_param}
      }.should raise_error
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

      it "redirects to the created external_service" do
        post :create, {:external_service => ExternalService.plan, :project_id => project.to_param}
        response.should redirect_to([project, ExternalService.last])
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

      it "re-renders the 'new' template" do
        ExternalService.any_instance.stub(:save).and_return(false)
        ExternalService.any_instance.stub(:errors).and_return(errors)
        post :create, {:external_service => {}, :project_id => project.to_param}
        response.should render_template("new")
      end
    end

    it "fails if the requested external_service is not in current account projects" do
      expect {
        post :create, {:project_id => other_external_service.project.to_param, :external_service => ExternalService.plan}
      }.should raise_error
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

      it "redirects to the external_service" do
        put :update, {:id => external_service.to_param, :external_service => ExternalService.plan, :project_id => project.to_param}
        response.should redirect_to([project, external_service])
      end
    end

    describe "with invalid params" do
      it "assigns the external_service as @external_service" do
        ExternalService.any_instance.stub(:save).and_return(false)
        put :update, {:id => external_service.to_param, :external_service => {}, :project_id => project.to_param}
        controller.external_service.should eq(external_service)
      end

      it "re-renders the 'edit' template" do
        ExternalService.any_instance.stub(:save).and_return(false)
        ExternalService.any_instance.stub(:errors).and_return(errors)
        put :update, {:id => external_service.to_param, :external_service => {}, :project_id => project.to_param}
        response.should render_template("edit")
      end
    end

    it "fails if the requested external_service is not in current account projects" do
      expect {
        put :update, {:id => other_external_service.to_param, :external_service => ExternalService.plan, :project_id => other_external_service.project.to_param}
      }.should raise_error
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested external_service" do
      expect {
        delete :destroy, {:id => external_service.to_param, :project_id => project.to_param}
      }.to change(ExternalService, :count).by(-1)
    end

    it "redirects to the external_services list" do
      delete :destroy, {:id => external_service.to_param, :project_id => project.to_param}
      response.should redirect_to(project_external_services_url(project))
    end

    it "fails if the requested external_service is not in current account projects" do
      expect {
        delete :update, {:id => other_external_service.to_param, :external_service => ExternalService.plan, :project_id => other_external_service.project.to_param}
      }.should raise_error
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

    it "redirects to the external_services list" do
      put :update_manifest, {:id => external_service.to_param, :project_id => project.to_param}
      response.should redirect_to(project_external_services_url(project))
    end

     it "fails if the requested external_service is not in current account projects" do
        expect {
          put :update_manifest, {:id => other_external_service.to_param, :project_id => other_external_service.project.to_param}
        }.should raise_error
      end
  end

end
