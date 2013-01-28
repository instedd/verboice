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

describe Voxeo::CallManager do

  let(:channel_id) { 123 }
  let(:voxeo_session_id) { 'abcd1234' }
  let(:session_id) { 345 }
  let(:caller_id) { 678 }
  let(:builder) { double('builder') }
  let(:context) { double('context') }
  let(:store) { double('store') }
  let(:session) { double('session')}
  let(:call_manager) { Voxeo::CallManager.new channel_id, voxeo_session_id, {:session_id => session_id, :caller_id => caller_id, :context => context} }

  before(:each) do
    Builders::Vxml.should_receive(:new).and_return(builder)
    HttpBroker::SessionStore.stub(:instance).and_return(store)
    store.stub(:session_for).with(voxeo_session_id).and_return(session)
    context.stub(:headers).and_return(:Host => 'requesthost.com')
  end

  it "should tell session id" do
    call_manager.session_id.should eq(session_id)
  end

  it "should tell voxeo session id" do
    call_manager.voxeo_session_id.should eq(voxeo_session_id)
  end

  it "should tell channel id" do
    call_manager.channel_id.should eq(channel_id)
  end

  it "should tell called id" do
    call_manager.caller_id.should eq(caller_id)
  end

  it "defaults answering machine to false" do
    call_manager.is_answering_machine?.should be_false
  end

  it "should build xml for play" do
    call_manager.should_receive(:sounds_url_for).with(:filename).and_return(:sound_url)
    builder.should_receive(:play).with(:sound_url).and_return(builder)
    call_manager.play(:filename)
  end

  it "should build xml for say" do
    builder.should_receive(:say).with("some text")
    call_manager.say("some text")
  end

  it "should build xml for pause" do
    builder.should_receive(:pause).with(13)
    call_manager.pause(13)
  end

  it "should tell sound path using data folder" do
    basename = "some/path/foo"
    call_manager.sound_path_for(basename).should eq(File.join(Rails.root, 'data', 'voxeo', "#{basename}.gsm"))
  end

  context "capture" do

    let(:options) { {:foo => "bar"} }

    before :each do
      call_manager.should_receive(:callback_url).and_return(:callback_url)
      expect_flush({:digits => "123"})
    end

    it "should build capture and callback xml" do
      builder.should_receive(:capture).with(options)
      builder.should_receive(:callback).with(:callback_url)

      call_manager.capture(options).should eq("123")
    end

    it 'should use sound url when options has play' do
      call_manager.should_receive(:sounds_url_for).with(:sound_path).and_return(:sound_url)
      options = {:play => :sound_path}
      expected_options = {:play => :sound_url}
      builder.should_receive(:capture).with(expected_options)
      builder.should_receive(:callback).with(:callback_url)

      call_manager.capture(options)
    end

  end

  context "hangup" do

    let(:fiber) { double('fiber') }

    before(:each) do
      builder.should_receive(:hangup)
      session.stub(:end!)
      expect_flush
    end

    it "should build hangup xml" do
      EM.stub(:next_tick)

      call_manager.hangup
    end

    it "should resume fiber" do
      Fiber.should_receive(:current).and_return(fiber)
      fiber.should_receive(:resume)

      EM.should_receive(:next_tick) do |&block|
        block.call
        true
      end

      call_manager.hangup
    end

    it 'should delete the session from the store' do
      EM.stub(:next_tick)
      session.should_receive(:end!)

      call_manager.hangup
    end

  end

  context "after hangup" do

    before(:each) do
      builder.should_receive(:hangup)
      EM.stub(:next_tick)
      session.stub(:end!)
      expect_flush
      call_manager.hangup
    end

    it "should not play" do
      builder.should_not_receive(:play)
      call_manager.play "foo"
    end

    it "should not capture" do
      builder.should_not_receive(:capture)
      call_manager.capture({})
    end

    it "should not say" do
      builder.should_not_receive(:say)
      call_manager.say("foo")
    end

    it "should not hangup" do
      builder.should_not_receive(:hangup)
      call_manager.hangup
    end

    it "should not pause" do
      builder.should_not_receive(:pause)
      call_manager.pause(5)
    end

  end

  context 'urls' do
    before(:each) do
      context.should_receive(:headers).and_return(:Host => 'requesthost.com')
    end

    it 'should tell callback_url using UrlHelper' do
      Voxeo::UrlHelper.should_receive(:callback_url).at_least(:once).with(:host => 'requesthost.com').and_return(:url)
      call_manager.send(:callback_url).should eq(:url)
    end

    it 'should tell audio_url using UrlHelper' do
      Guid.should_receive(:new).and_return(double('guid', :to_s => 'guid'))
      session.should_receive(:store).with('guid', :filename)
      Voxeo::UrlHelper.should_receive(:audio_url).with('guid', {:sessionid => voxeo_session_id, :host => 'requesthost.com'}).and_return(:url)
      call_manager.send(:sounds_url_for, :filename).should eq(:url)
    end
  end

  def expect_flush(response = {})
    context = double('context', :params => response)
    builder.should_receive(:build).and_return("xml")
    Fiber.should_receive(:yield).with("xml").and_return(context)
  end

end