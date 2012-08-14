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
  describe PlayFileCommand do

    before(:each) do
      @key = 'file_key'
      @file_path = "/path/to/original/file"
      @target_path = '/path/to/pbx/target_path'

      @session = Session.new :pbx => double('pbx'), :call_flow => double('call_flow').tap { |d| d.stub(:id => 1) }
      @session.pbx.should_receive(:sound_path_for).with("1-#{@key}").and_return(@target_path)
      @session.pbx.should_receive(:play).with(@target_path, anything)
      @session.recording_manager.should_receive(:recording_path_for).at_least(:once).with(@key).and_return(@file_path)
      @session.stub(:info)
      @session.stub(:trace)
    end

    let(:command) do
      PlayFileCommand.new @key
    end

    it "should convert the file if not present in target path" do
      File.should_receive(:exists?).with(@target_path).and_return(false)

      command.should_receive(:convert_to_8000_hz_gsm).with(@file_path, @target_path)
      command.run(@session)
    end

    it "should convert the file if target path is older" do
      File.should_receive(:exists?).with(@target_path).and_return(true)
      File.should_receive(:mtime).with(@target_path).and_return(Time.now.utc - 10.hours)
      File.should_receive(:mtime).with(@file_path).and_return(Time.now.utc)

      command.should_receive(:convert_to_8000_hz_gsm).with(@file_path, @target_path)
      command.run(@session)
    end

    it "should not convert the file if target path is newer" do
      File.should_receive(:exists?).with(@target_path).and_return(true)
      File.should_receive(:mtime).with(@target_path).and_return(Time.now.utc)
      File.should_receive(:mtime).with(@file_path).and_return(Time.now.utc - 10.hours)

      command.should_not_receive(:convert_to_8000_hz_gsm)
      command.run(@session)
    end

  end
end
