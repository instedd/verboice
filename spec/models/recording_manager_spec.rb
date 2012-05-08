require 'spec_helper'

describe RecordingManager do

  let(:application) { Application.make }
  let(:call_log) { CallLog.make }

  it "should retrieve an absolute path for a recording of an aplication" do
    manager = RecordingManager.for application
    manager.recording_path_for(1, 'foo').should eq(File.join Rails.root, "data","applications", "#{application.id}" ,"recordings", "1-foo.wav")
  end

  it "should retrieve an absolute path for a result of a call_log" do
    manager = RecordingManager.for call_log
    manager.result_path_for(2).should eq(File.join Rails.root, "data","call_logs", "#{call_log.id}" ,"results", "2.wav")
  end
end