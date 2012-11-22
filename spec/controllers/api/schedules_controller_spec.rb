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

describe Api::SchedulesController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let(:schedule) { Schedule.make project: project }

  it "should list all the schedules" do
    schedule
    get :index, :project_id => project.id

    response = JSON.parse(@response.body)
    response.size.should == 1
    response[0].with_indifferent_access[:name].should == schedule.name
  end

  it "should expose an schedule" do
    get :show, :project_id => project.id, :name => schedule.name

    response = JSON.parse(@response.body).with_indifferent_access
    response[:name].should == schedule.name
  end

  it "create custom schedule" do
    data = {project_id: project.id, name: "foo", :time_from_str => Time.now.to_s, :time_to_str => (Time.now + 1.hour).to_s}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, project_id: project.id, format: :json

    assert_response :ok
    schedules = project.schedules.all
    schedules.size.should == 1
    schedules[0].name.should == data[:name]
  end

  it "should response with the creation errors" do
    data = { project_id: project.id }
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, project_id: project.id, format: :json
    assert_response :ok

    project.schedules.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the Schedule"
    response[:properties].should == [{"name" => "can't be blank"}, {"time_from"=>"can't be blank"}, {"time_to"=>"can't be blank"}]
  end

  it "should delete an schedule" do
    delete :destroy, :name => schedule.name, :project_id => project.id
    assert_response :ok

    project.schedules.count.should == 0
  end
end
