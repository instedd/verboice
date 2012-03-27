require 'spec_helper'

describe PlayUrlCommand do
  def setup_for_url(url)
    @url = url
    Digest::MD5.should_receive(:hexdigest).with(@url).and_return(:md5)

    @session = Session.new :pbx => mock('pbx')
    @session.pbx.should_receive(:sound_path_for).with(:md5).and_return(:target_path)
    @session.pbx.should_receive(:play).with(:target_path)
    @session.should_receive(:info).with("Play #{@url}")
  end

  it "don't download if already downloaded" do
    setup_for_url 'http://foo.gsm'

    File.should_receive(:exists?).with(:target_path).and_return(true)

    @session.should_receive(:trace).with("File #{:target_path} already exists")

    cmd = PlayUrlCommand.new @url
    cmd.should_receive(:download_url_to).never
    cmd.run @session
  end

  it "download mp3 converts first to wav then to gsm" do
    setup_for_url 'http://foo.mp3'

    File.should_receive(:exists?).with(:target_path).and_return(false)
    File.should_receive(:is_mpeg?).with(:tmp_file).and_return(true)

    @session.should_receive(:trace).with("Download #{@url}")

    cmd = PlayUrlCommand.new @url
    cmd.should_receive(:download_url_to_temporary_location).and_yield(:tmp_file)
    cmd.should_receive(:convert_to_wav).with(:tmp_file)
    cmd.should_receive(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end

  def test_download_wav_converts_to_gsm
    setup_for_url 'http://foo.wav'

    File.should_receive(:exists?).with(:target_path).and_return(false)

    @session.should_receive(:trace).with("Download #{@url}")

    cmd = PlayUrlCommand.new @url
    cmd.should_receive(:download_url_to_temporary_location).and_yield(:tmp_file)
    cmd.should_receive(:convert_to_wav).never
    cmd.should_receive(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end

  it "download wav converts to gsm" do
    test_download_wav_converts_to_gsm
  end

  it "download gsm still converts to gsm to make it 8000 hz" do
    test_download_wav_converts_to_gsm
    setup_for_url 'http://foo.gsm'

    File.should_receive(:exists?).with(:target_path).and_return(false)

    @session.should_receive(:trace).with("Download #{@url}")

    cmd = PlayUrlCommand.new @url
    cmd.should_receive(:download_url_to_temporary_location).and_yield(:tmp_file)
    cmd.should_receive(:convert_to_wav).never
    cmd.should_receive(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
    cmd.run @session
  end
end
