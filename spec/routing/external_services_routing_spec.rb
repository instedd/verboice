require "spec_helper"

describe ExternalServicesController do
  describe "routing" do

    it "routes to #index" do
      get("/projects/3/external_services").should route_to("external_services#index", :project_id => "3")
    end

    it "routes to #new" do
      get("/projects/3/external_services/new").should route_to("external_services#new", :project_id => "3")
    end

    it "routes to #show" do
      get("/projects/3/external_services/1").should route_to("external_services#show", :project_id => "3", :id => "1")
    end

    it "routes to #edit" do
      get("/projects/3/external_services/1/edit").should route_to("external_services#edit", :project_id => "3", :id => "1")
    end

    it "routes to #create" do
      post("/projects/3/external_services").should route_to("external_services#create", :project_id => "3")
    end

    it "routes to #update" do
      put("/projects/3/external_services/1").should route_to("external_services#update", :project_id => "3", :id => "1")
    end

    it "routes to #destroy" do
      delete("/projects/3/external_services/1").should route_to("external_services#destroy", :project_id => "3", :id => "1")
    end

    it "routes to #update_manifest" do
      put("/projects/3/external_services/1/update_manifest").should route_to("external_services#update_manifest", :project_id => "3", :id => "1")
    end

  end
end
