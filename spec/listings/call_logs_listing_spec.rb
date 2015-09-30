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

RSpec.describe CallLogsListing, type: :listing do
  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }

  before(:each) do
    CallLogsListing.test_account = account
    # sign_in account
  end

  let(:listing) { query_listing :call_logs }

  it 'should get call logs' do
    calls = 10.times.map { CallLog.make :channel => channel }
    items = listing.items.to_a
    expected = calls.sort_by(&:id).reverse.to_a
    items.should == expected
  end

  it 'should render' do
    CallLog.make channel: channel
    CallLog.make channel: channel, state: 'queued'
    render_listing :call_logs
  end
end
