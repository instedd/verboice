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

describe Api::ProjectsController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { account.projects.make }
  let!(:call_flow) { project.call_flows.make }
  let!(:schedule) { project.schedules.make }

  it "should list all projects" do
    get :index

    response = JSON.parse(@response.body)
    response.length.should == 1
    response[0]['id'].should eq(project.id)
    response[0]['name'].should eq(project.name)
    response[0]['call_flows'].length.should eq(1)
    response[0]['call_flows'][0]['id'].should eq(call_flow.id)
    response[0]['call_flows'][0]['name'].should eq(call_flow.name)
    response[0]['schedules'].length.should eq(1)
    response[0]['schedules'][0].should eq(schedule.name)
  end
end
