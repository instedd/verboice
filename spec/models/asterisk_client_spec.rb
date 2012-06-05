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

describe Asterisk::Client do
  context "receiving events" do
    before(:each) do
      @ami = Asterisk::Client.new 1
      @session = Session.new :call_log => CallLog.make
    end

    it "receive event originate response failure fails with busy" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :busy)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '5'
    end

    it "receive event originate response failure fails with no answer" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :no_answer)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => '3'
    end

    it "receive event originate response failure fails with generic failure" do
      BaseBroker.instance = mock 'broker'
      BaseBroker.instance.should_receive(:call_rejected).with(@session.id, :failed)

      @ami.receive_event :event => 'OriginateResponse', :response => 'Failure', :actionid => @session.id.to_s, :reason => 'X'
    end

    it "receive originate response without failure ignores it" do
      @ami.receive_event :event => 'OriginateResponse', :response => 'Success', :actionid => @session.id.to_s

      @session.call_log.reload
      @session.call_log.state.should == :active
    end

    it "receive other failure event ignores it" do
      @ami.receive_event :event => 'SomeOtherEvent', :response => 'Failure', :actionid => @session.id.to_s

      @session.call_log.reload
      @session.call_log.state.should == :active
    end
  end
end

