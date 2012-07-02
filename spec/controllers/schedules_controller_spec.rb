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

describe SchedulesController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:schedule) { Schedule.make project: project }

  describe "GET index" do
    it "assigns all schedules as @schedules" do
      get :index, {:project_id => project.to_param}
      controller.schedules.should eq([schedule])
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new Schedule" do
        expect {
          post :create, {:schedule => Schedule.plan, :project_id => project.to_param}
        }.to change(Schedule, :count).by(1)
      end

      it "assigns a newly created schedule as @schedule" do
        post :create, {:schedule => Schedule.plan, :project_id => project.to_param}
        controller.schedule.should be_a(Schedule)
        controller.schedule.should be_persisted
      end

      it "renders 'box_content' template" do
        post :create, {:schedule => Schedule.plan, :project_id => project.to_param}
        response.should render_template('box_content')
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved schedule as @schedule" do
        Schedule.any_instance.stub(:save).and_return(false)
        post :create, {:schedule => {}, :project_id => project.to_param}
        controller.schedule.should be_a_new(Schedule)
      end

      it "re-renders the 'box_content' template" do
        Schedule.any_instance.stub(:save).and_return(false)
        post :create, {:schedule => {}, :project_id => project.to_param}
        response.should render_template("box_content")
      end
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested schedule" do
        put :update, {:project_id => project.id, :id => schedule.to_param, :schedule => {:name => 'new name'}}
        schedule.reload.name.should eq('new name')
      end

      it "assigns the requested schedule as @schedule" do
        put :update, {:id => schedule.to_param, :schedule => Schedule.plan, :project_id => project.to_param}
        controller.schedule.should eq(schedule)
      end

      it "renders 'box_content' template" do
        put :update, {:id => schedule.to_param, :schedule => Schedule.plan, :project_id => project.to_param}
        response.should render_template('box_content')
      end
    end

    describe "with invalid params" do
      it "assigns the schedule as @schedule" do
        Schedule.any_instance.stub(:save).and_return(false)
        put :update, {:id => schedule.to_param, :schedule => {}, :project_id => project.to_param}
        controller.schedule.should eq(schedule)
      end

      it "re-renders the 'box_content' template" do
        Schedule.any_instance.stub(:save).and_return(false)
        put :update, {:id => schedule.to_param, :schedule => {}, :project_id => project.to_param}
        response.should render_template("box_content")
      end
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested schedule" do
      expect {
        delete :destroy, {:id => schedule.to_param, :project_id => project.to_param}
      }.to change(Schedule, :count).by(-1)
    end

    it "redirects to the schedules list" do
      delete :destroy, {:id => schedule.to_param, :project_id => project.to_param}
      response.should redirect_to(project_schedules_path(project))
    end
  end
end