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

describe RecordingManager do

  let(:project) { Project.make }
  let(:call_log) { CallLog.make }

  it "should retrieve an absolute path for a recording of an aplication" do
    manager = RecordingManager.for project
    manager.recording_path_for('key').should eq(File.join Rails.root, "data","projects", "#{project.id}" ,"recordings", "key.wav")
  end

  it "should retrieve an absolute path for a result of a call_log" do
    manager = RecordingManager.for call_log
    manager.result_path_for(2).should eq(File.join Rails.root, "data","call_logs", "#{call_log.id}" ,"results", "2.wav")
  end

  it "should format recording" do
    RecordingManager.format_recording(12345, 'explanation').should eq('12345-explanation')
  end
end