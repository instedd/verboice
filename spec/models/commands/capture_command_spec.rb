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
    before(:each) do
      @defaults = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
      @session = Session.new :pbx => mock('pbx')
      @digit = :digit
    end

    def expect_capture(options = {})
      options = @defaults.merge(options)

      if options[:play]
        @session.should_receive(:log).with(
          :info => "Play file #{options[:play]}. Waiting user input",
          :trace => "Play file #{options[:play]}. Waiting user input: #{options.to_pretty_s}"
        )
      else
        @session.should_receive(:log).with(
          :info => "Waiting user input",
          :trace => "Waiting user input: #{options.to_pretty_s}"
        )
      end
      @session.pbx.should_receive(:capture).with(options.merge(:after_play => anything)).and_return(@digit)
      case @digit
      when nil
        @session.should_receive(:info).with("User didn't press enough digits")
      when :timeout
        @session.should_receive(:info).with("User timeout")
      when :finish_key
        @session.should_receive(:info).with("User pressed the finish key")
      else
        @session.should_receive(:info).with("User pressed: #{@digit}")
      end
    end

    it "capture returns next command" do
      expect_capture

      cmd = CaptureCommand.new
      cmd.next = :next
      cmd.run(@session).should == :next
    end

    it "capture one key" do
      expect_capture

      CaptureCommand.new.run @session

      @session[:digits].should == :digit
      @session[:timeout].should be_false
      @session[:finish_key].should be_false
    end

    it "capture one key timeout" do
      @digit = :timeout
      expect_capture

      CaptureCommand.new.run @session

      @session[:digits].should be_nil
      @session[:timeout].should_not be_nil
      @session[:finish_key].should be_false
    end

    it "capture one key finish key" do
      @digit = :finish_key
      expect_capture

      CaptureCommand.new.run @session

      @session[:digits].should be_nil
      @session[:timeout].should be_false
      @session[:finish_key].should_not be_nil
    end

    it "capture at least two keys" do
      expect_capture :min => 2, :max => Float::INFINITY

      CaptureCommand.new(:min => 2).run @session

      @session[:digits].should == @digit
    end

    it "capture at most three keys" do
      expect_capture :max => 3

      CaptureCommand.new(:max => 3).run @session

      @session[:digits].should == @digit
    end

    it "capture exactly four keys" do
      expect_capture :min => 4, :max => 4

      CaptureCommand.new(:min => 4, :max => 4).run @session

      @session[:digits].should == @digit
    end

    it "capture with timeout" do
      expect_capture :timeout => 1

      CaptureCommand.new(:timeout => 1).run @session

      @session[:digits].should == @digit
    end

    it "capture with finish on key" do
      expect_capture :finish_on_key => '*'

      CaptureCommand.new(:finish_on_key => '*').run @session

      @session[:digits].should == @digit
    end

    it "capture with play empty" do
      expect_capture :finish_on_key => '*'

      CaptureCommand.new(:finish_on_key => '*', :play => ' ').run @session

      @session[:digits].should == @digit
    end

    it "capture with play" do
      @session.should_receive(:log).with(
        :info => "Play file url. Waiting user input",
        :trace => "Play file url. Waiting user input: #{@defaults.merge(:play => :url).to_pretty_s}"
      )
      @session.pbx.should_receive(:capture).with(@defaults.merge(:play => :target_path, :after_play => anything)).and_return(@digit)
      @session.should_receive(:info).with("User pressed: #{@digit}")


      play = mock('play')
      play.should_receive(:download).with(@session).and_return(:target_path)
      PlayUrlCommand.should_receive(:new).with(:url).and_return(play)

      CaptureCommand.new(:play => :url).run @session

      @session[:digits].should == @digit
    end

    it "capture with say" do
      @session.should_receive(:log).with(
        :info => "Say 'some text'. Waiting user input",
        :trace => "Say 'some text'. Waiting user input: #{@defaults.merge(:say => "some text").to_pretty_s}"
      )
      @session.pbx.should_receive(:capture).with(@defaults.merge(:say => "some text", :after_play => anything)).and_return(@digit)
      @session.should_receive(:info).with("User pressed: #{@digit}")

      CaptureCommand.new(:say => "some text").run @session

      @session[:digits].should == @digit
    end
  end
end