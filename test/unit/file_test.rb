require 'test_helper'

class FilesTest < ActiveSupport::TestCase

  test "get mime type of mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    assert_equal 'audio/mpeg', File.mime_type(mp3_path)
  end

  test "get mime type of wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    assert_equal 'audio/x-wav', File.mime_type(wav_path)
  end

  test "detect mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    assert_true File.is_mpeg?(mp3_path)
  end

  test "detect wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    assert_true File.is_wav?(wav_path)
  end

end
