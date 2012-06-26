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

describe BrokerFacade do
  Channel.all_leaf_subclasses.each do |a_channel|
    before(:each) { BaseBroker.instance = mock 'broker', :pbx_available? => true }
    let(:facade) { BrokerFacade.new 1 }
    let(:channel) { a_channel.make }

    [:notify_call_queued, :create_channel, :delete_channel].each do |method|
      it "#{method} for" do
        BaseBroker.instance.should_receive(method).with channel
        facade.send method, channel.id
      end
    end

    it "schedule delayed call" do
      time = Time.now.utc + 2.hours
      facade.notify_call_queued channel.id, time
    end
  end
end