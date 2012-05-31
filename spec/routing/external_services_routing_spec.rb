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

  end
end
