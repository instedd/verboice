require 'spec_helper'

describe File do
  it "get mime type of mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    File.mime_type(mp3_path).should == 'audio/mpeg'
  end

  it "get mime type of wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    File.mime_type(wav_path).should == 'audio/x-wav'
  end

  it "detect mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    File.is_mpeg?(mp3_path).should == true
  end

  it "detect wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    File.is_wav?(wav_path).should == true
  end

end
