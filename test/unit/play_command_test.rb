require 'test_helper'

class PlayCommandTest < ActiveSupport::TestCase
  def setup_for_url(url)
    @url = url
    Digest::MD5.expects(:hexdigest).with(@url).returns(:md5)

    @session = Session.new :pbx => mock('pbx')
    @session.pbx.expects(:sound_path_for).with(:md5).returns(:target_path)
    @session.pbx.expects(:play).with(:target_path)
  end

  test "don't download if already downloaded" do
    setup_for_url 'http://foo.gsm'

    File.expects(:exists?).with(:target_path).returns(true)

    cmd = PlayCommand.new @url
    cmd.expects(:download_url_to).never
    cmd.run @session
  end

  test "download mp3 converts first to wav then to gsm" do
    setup_for_url 'http://foo.mp3'

    File.expects(:exists?).with(:target_path).returns(false)
    File.expects(:is_mpeg?).with(:tmp_file).returns(true)

    cmd = PlayCommand.new @url
    cmd.expects(:download_url_to_temporary_location).yields(:tmp_file)
    cmd.expects(:convert_to_wav).with(:tmp_file)
    cmd.expects(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end

  test "download wav converts to gsm" do
    setup_for_url 'http://foo.wav'

    File.expects(:exists?).with(:target_path).returns(false)

    cmd = PlayCommand.new @url
    cmd.expects(:download_url_to_temporary_location).yields(:tmp_file)
    cmd.expects(:convert_to_wav).never
    cmd.expects(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end

  test "download gsm still converts to gsm to make it 8000 hz" do
    test_download_wav_converts_to_gsm
    setup_for_url 'http://foo.gsm'

    File.expects(:exists?).with(:target_path).returns(false)

    cmd = PlayCommand.new @url
    cmd.expects(:download_url_to_temporary_location).yields(:tmp_file)
    cmd.expects(:convert_to_wav).never
    cmd.expects(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end
end
