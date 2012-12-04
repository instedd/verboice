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

describe Freeswitch::Broker do
  before(:each) do
    @broker = Freeswitch::Broker.new
    @broker.sessions.clear
    @broker.freeswitch_client = mock 'freeswitch_client'
    @channel = Channels::Custom.make
  end

  let(:queued_call) { @channel.queued_calls.make }

  it "returns channels" do
    Channels::Voxeo.make
    @broker.channels.should eq([@channel])
  end

  it "call ok" do
    @broker.freeswitch_client.should_receive(:error?).and_return(false)
    @broker.freeswitch_client.should_receive(:command).with("bgapi originate {verboice_channel_id=#{@channel.id},verboice_call_log_id=#{queued_call.call_log.id}}#{queued_call.address} '&socket(localhost:#{Freeswitch::CallManager::Port} sync full)'")

    result = @broker.call queued_call
    result.should == nil
  end

  it "call fails on pbx error" do
    @broker.freeswitch_client.should_receive(:error?).and_return(true)

    ex = assert_raise(RuntimeError) { @broker.call queued_call }
    assert_match /not available/, ex.message
  end
end
