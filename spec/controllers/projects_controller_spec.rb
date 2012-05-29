require 'spec_helper'

describe ProjectsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  context "CRUD" do

    let!(:project) do
      Project.make :name => 'Project Name', :account => @account
    end

    it "should edit a project" do
      get :edit, :id => project.id
      response.should be_successful
      assigns(:project).should eq(project)
    end

    it "should update a project" do
      put :update, :id => project.id, :project => {:name => 'My New Project Name', :time_zone => 'GMT-3'}
      response.should be_redirect
      project.reload.name.should eq('My New Project Name')
      project.reload.time_zone.should eq('GMT-3')
    end

  end
end
