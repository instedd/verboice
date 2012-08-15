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
  describe CaptureCommand do

    let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}
    let(:digits) { :digits }

    def expect_capture(options = {}, digit = digits)
      defaults = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5, :after_play => anything, :if_hang_up => anything}
      session.pbx.should_receive(:capture).with(defaults.merge(options)).and_return(digit)
    end

    it "capture returns next command" do
      expect_capture

      cmd = CaptureCommand.new
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "capture one key" do
      expect_capture

      CaptureCommand.new.run session

      session[:digits].should == digits
      session[:timeout].should be_false
      session[:finish_key].should be_false
    end

    it "capture one key timeout" do
      expect_capture({}, :timeout)

      CaptureCommand.new.run session

      session[:digits].should be_nil
      session[:timeout].should_not be_nil
      session[:finish_key].should be_false
    end

    it "capture one key finish key" do
      expect_capture({}, :finish_key)

      CaptureCommand.new.run session

      session[:digits].should be_nil
      session[:timeout].should be_false
      session[:finish_key].should_not be_nil
    end

    it "capture at least two keys" do
      expect_capture :min => 2, :max => Float::INFINITY

      CaptureCommand.new(:min => 2).run session

      session[:digits].should == digits
    end

    it "capture at most three keys" do
      expect_capture :max => 3

      CaptureCommand.new(:max => 3).run session

      session[:digits].should == digits
    end

    it "capture exactly four keys" do
      expect_capture :min => 4, :max => 4

      CaptureCommand.new(:min => 4, :max => 4).run session

      session[:digits].should == digits
    end

    it "capture with timeout" do
      expect_capture :timeout => 1

      CaptureCommand.new(:timeout => 1).run session

      session[:digits].should == digits
    end

    it "capture with finish on key" do
      expect_capture :finish_on_key => '*'

      CaptureCommand.new(:finish_on_key => '*').run session

      session[:digits].should == digits
    end

    it "capture with play empty" do
      expect_capture :finish_on_key => '*'

      CaptureCommand.new(:finish_on_key => '*', :play => ' ').run session

      session[:digits].should == digits
    end

    it "capture with play" do
      expect_capture :play => :target_path

      play = mock('play')
      play.should_receive(:download).with(session).and_return(:target_path)
      PlayUrlCommand.should_receive(:new).with(:url).and_return(play)

      CaptureCommand.new(:play => :url).run session

      session[:digits].should == digits
    end

    it "capture with say" do
      expect_capture :say => "some text"

      CaptureCommand.new(:say => "some text").run session

      session[:digits].should == digits
    end

    it "capture with resource" do
      expect_capture :say => "some text"
      localized = TextLocalizedResource.make text: 'some text'
      session.call_flow = CallFlow.make project: localized.project

      CaptureCommand.new(:resource => localized.resource.guid).run session

      session[:digits].should == digits
    end
  end
end
