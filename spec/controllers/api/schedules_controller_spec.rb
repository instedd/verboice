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
    expect(response.size).to eq(1)
    expect(response[0].with_indifferent_access[:name]).to eq(schedule.name)
  end

  it "should expose an schedule" do
    get :show, :project_id => project.id, :name => schedule.name

    response = JSON.parse(@response.body).with_indifferent_access
    expect(response[:name]).to eq(schedule.name)
  end

  it "create custom schedule" do
    data = {project_id: project.id, name: "foo", :time_from_str => Time.gm(2000, 1, 1, 10, 0), :time_to_str => Time.gm(2000, 1, 1, 11, 0)}
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, project_id: project.id, format: :json

    assert_response :ok
    response = JSON.parse(@response.body).with_indifferent_access
    expect(response[:name]).to eq("foo"), "Expected response to contain schedule name 'foo', but was: #{@response.body}"

    schedules = project.schedules.all
    expect(schedules.size).to eq(1)
    expect(schedules[0].name).to eq(data[:name])
  end

  it "should response with the creation errors" do
    data = { project_id: project.id }
    @request.env['RAW_POST_DATA'] = data.to_json
    post :create, project_id: project.id, format: :json
    assert_response :ok

    expect(project.schedules.count).to eq(0)

    response = JSON.parse(@response.body).with_indifferent_access
    expect(response[:summary]).to eq("There were problems creating the Schedule")
    expect(response[:properties]).to eq([{"name" => "can't be blank"}, {"time_from"=>"can't be blank"}, {"time_to"=>"can't be blank"}])
  end

  it "should delete an schedule" do
    delete :destroy, :name => schedule.name, :project_id => project.id
    assert_response :ok

    expect(project.schedules.count).to eq(0)
  end
end
