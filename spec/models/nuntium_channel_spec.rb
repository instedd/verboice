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

describe NuntiumChannel do
  let(:account) { Account.make }

  it "should create a new Pigeon channel when it's a new record" do
    nc = NuntiumChannel.new
    nc.channel.should_not be_nil
  end

  it "should create a Pigeon channel of the given kind" do
    nc = NuntiumChannel.new kind: 'foo'
    nc.channel.kind.should eq('foo')
  end

  it "kind should be readonly once set initially" do
    nc = NuntiumChannel.new kind: 'foo'
    nc.kind = 'bar'
    nc.kind.should eq('foo')
  end

  it "should set channel_name before validating the record" do
    nc = NuntiumChannel.new kind: 'foo'
    nc.channel_name.should be_blank
    nc.should_not be_valid
    nc.channel_name.should_not be_blank
    nc.channel_name.should == nc.channel.name
  end

  it "should save the Pigeon channel when saving the record" do
    nc = NuntiumChannel.new kind: 'foo', name: 'bar'
    nc.account = account
    nc.channel.should_receive(:save!)
    nc.save.should be_true
  end

  it "should find the Pigeon channel if channel_name is present (ie. it's a persisted record)" do
    nc = NuntiumChannel.new kind: 'foo', name: 'bar'
    nc.account = account
    nc.channel.should_receive(:save!)
    nc.save.should be_true

    nc2 = NuntiumChannel.find(nc.id)
    Pigeon::NuntiumChannel.should_receive(:find).with(nc.channel_name).and_return(nc.channel)
    nc2.channel.should == nc.channel
  end

  it "should create the Pigeon channel with account restrictions and propagate enabled status" do
    nc = NuntiumChannel.new kind: 'foo', name: 'bar', enabled: false
    nc.account = account
    nc.channel.should_receive(:save!)
    nc.save.should be_true

    nc.channel.restrictions.should include({ "name" => "account_id", "value" => account.id.to_s })
    nc.channel.should_not be_enabled
  end

  it "should destroy the Pigeon channel when destroying the record" do
    nc = NuntiumChannel.new kind: 'foo', name: 'bar'
    nc.account = account
    nc.channel.should_receive(:save!)
    nc.save.should be_true

    nc.channel.should_receive(:destroy)
    nc.destroy
  end

  it "save should not raise an exception when there is a Pigeon validation error" do
    nc = NuntiumChannel.new kind: 'foo', name: 'bar'
    nc.account = account
    nc.channel.should_receive(:save!).and_raise(Pigeon::ChannelInvalid.new(nc.channel))
    nc.save.should be_false
  end
end
