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

describe CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }

  before(:each) do
    sign_in account
  end

  it 'should get queued calls' do
    calls = 10.times.map { QueuedCall.make :channel => channel }
    get :queued
    response.should be_success
    assigns(:calls).should eq(calls.sort_by(&:id).reverse)
  end
end
