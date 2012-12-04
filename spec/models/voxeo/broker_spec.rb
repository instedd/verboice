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

describe Voxeo::Broker do
  let(:broker) { Voxeo::Broker.new }
  let!(:channel) { Channels::Voxeo.make }

  it "returns channels" do
    Channels::Custom.make
    broker.channels.should eq([channel])
  end

  it "defaults pbx available to true" do
    broker.pbx_available?.should be_true
  end

  context "call" do
    let(:session) { Session.new :channel => channel, :address => 'foo' }

    it "should make a call request" do
      expect_em_http :get, channel.url, :with => {:timeout => Voxeo::Broker::TIMEOUT,:query => {:tokenid => channel.token, :callsid => session.id, :numbertodial => session.address}}
      broker.call session
    end
  end
end