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
  describe PlayUrlCommand do
    def setup_for_url(url)
      @url = url
      Digest::MD5.should_receive(:hexdigest).with(@url).and_return(:md5)

      @session = Session.new pbx: double('pbx'), call_log: CallLog.make
      @session.pbx.should_receive(:sound_path_for).with(:md5).and_return(:target_path)
      @session.pbx.should_receive(:play).with(:target_path, :if_hang_up => anything)
      @session.stub(:trace)
    end

    it "don't download if already downloaded" do
      setup_for_url 'http://foo.gsm'

      File.should_receive(:exists?).with(:target_path).and_return(true)

      cmd = PlayUrlCommand.new @url
      cmd.should_receive(:download_url_to).never
      cmd.next = :next
      cmd.run(@session).should == :next
    end

    it "download mp3 converts first to wav then to gsm" do
      setup_for_url 'http://foo.mp3'

      File.should_receive(:exists?).with(:target_path).and_return(false)
      File.should_receive(:is_mpeg?).with(:tmp_file).and_return(true)

      cmd = PlayUrlCommand.new @url
      cmd.should_receive(:download_url_to_temporary_location).and_yield(:tmp_file)
      cmd.should_receive(:convert_to_wav).with(:tmp_file)
      cmd.should_receive(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
      cmd.run @session
    end

    def test_download_wav_converts_to_gsm
      setup_for_url 'http://foo.wav'

      File.should_receive(:exists?).with(:target_path).and_return(false)

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

      cmd = PlayUrlCommand.new @url
      cmd.should_receive(:download_url_to_temporary_location).and_yield(:tmp_file)
      cmd.should_receive(:convert_to_wav).never
      cmd.should_receive(:convert_to_8000_hz_gsm).with(:tmp_file, :target_path)
      cmd.run @session
    end
  end
end