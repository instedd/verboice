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

describe Freeswitch::Adapter do
  before(:each) do
    @context = mock('context')
    @adapter = Freeswitch::Adapter.new @context
  end

  [
    [:channel_id, :variable_verboice_channel_id],
    [:call_log_id, :variable_verboice_call_log_id],
    [:caller_id, :variable_effective_caller_id_number]
  ].each do |method, key|
    it "#{method}" do
      @context.stub(:session => {key => :id})
      @adapter.send(method).should == :id
    end
  end

  [:answer, :hangup].each do |cmd|
    it cmd.to_s do
      @context.should_receive(cmd)
      @adapter.send cmd
    end
  end

  it "play" do
    @context.should_receive(:playback).with(:url)
    @adapter.play :url
  end

  it 'pauses' do
    EM.should_receive(:fiber_sleep).with(13)
    @adapter.pause(13)
  end

  it 'say' do
    session = stub('session')
    session.should_receive(:synth).with('some text').and_return(:filename)

    @adapter.session = session
    @adapter.should_receive(:play).with(:filename)

    @adapter.say 'some text'
  end

  context "capture" do
    it "capture one digit" do
      @context.should_receive(:read).with('silence_stream://1', :min => 1, :max => 1, :terminators => '', :timeout => 5000, :variable => 'capture').and_return('1')
      value = @adapter.capture :min => 1, :max => 1, :finish_on_key => '', :timeout => 5
      value.should == '1'
    end

    it "capture non default options" do
      @context.should_receive(:read).with('silence_stream://1', :min => 2, :max => 5, :terminators => '*', :timeout => 3000, :variable => 'capture').and_return('1')
      value = @adapter.capture :min => 2, :max => 5, :finish_on_key => '*', :timeout => 3
      value.should == '1'
    end

    it "capture while playing" do
      @context.should_receive(:read).with('some_file', :min => 2, :max => 5, :terminators => '*', :timeout => 3000, :variable => 'capture').and_return('1')
      value = @adapter.capture :min => 2, :max => 5, :finish_on_key => '*', :timeout => 3, :play => 'some_file'
      value.should == '1'
    end
  end
end
