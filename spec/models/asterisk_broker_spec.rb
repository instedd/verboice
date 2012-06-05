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

describe Asterisk::Broker do
  before(:each) do
    @broker = Asterisk::Broker.new
    $asterisk_client = mock('asterisk_client')
    @channel = Channel.make :kind => 'custom', :config => {'dial_string' => 'SIP/{number}'}
  end

  context "call" do
    before(:each) do
      @session = Session.new :channel => @channel, :address => 'Foo'
    end

    it "call ok" do
      $asterisk_client.should_receive(:error?).and_return(false)
      $asterisk_client.should_receive(:originate).with({
        :channel => "SIP/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).and_return(:response => 'OK')

      result = @broker.call @session
      result.should == nil
    end

    it "call fails on asterisk_client error" do
      $asterisk_client.should_receive(:error?).and_return(true)

      ex = assert_raise(PbxUnavailableException) { @broker.call @session }
      assert_match /not available/, ex.message
    end

    it "call fails on originate error" do
      $asterisk_client.should_receive(:error?).and_return(false)
      $asterisk_client.should_receive(:originate).with({
        :channel => "SIP/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).and_return(:response => 'Error', :message => 'Oops')

      ex = assert_raise(RuntimeError) { @broker.call @session }
      ex.message.should == 'Oops'
    end
  end
end
