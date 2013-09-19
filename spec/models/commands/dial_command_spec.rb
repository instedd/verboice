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

module Commands
  describe DialCommand do
    it "run" do
      dial = DialCommand.new '1234'
      dial.next = :next
      session = Session.new :channel => Channels::CustomSip.make

      broker = mock('broker')
      session.stub(:broker) { broker }
      session.broker.should_receive(:get_dial_address).with(session.channel, '1234').and_return('SIP/1234')
      session.call_log = CallLog.make
      session.pbx = mock('pbx')
      session.pbx.should_receive(:dial).with('SIP/1234', {}).and_return(:completed)
      dial.run(session).should == :next
      session[:dial_status].should == :completed
    end

    it "run with channel" do
      account = Account.make
      channel = Channels::CustomSip.make :account => account
      session = Session.new :channel => channel
      dial = DialCommand.new '1234', :channel => channel.name

      broker = mock('broker')
      session.stub(:broker) { broker }
      session.broker.should_receive(:get_dial_address).with(channel, '1234').and_return('SIP/1234')
      session.call_log = CallLog.make
      session.pbx = mock('pbx')
      session.pbx.should_receive(:dial).with('SIP/1234', {})
      dial.run session
    end

    it "run with custom caller id" do
      dial = DialCommand.new '1234', :caller_id => 'foo'
      session = Session.new :channel => Channels::CustomSip.make

      broker = mock('broker')
      session.stub(:broker) { broker }
      session.broker.should_receive(:get_dial_address).with(session.channel, '1234').and_return('SIP/1234')
      session.call_log = CallLog.make
      session.pbx = mock('pbx')
      session.pbx.should_receive(:dial).with('SIP/1234', {:caller_id => 'foo'}).and_return(:completed)
      dial.run session
    end
  end
end
