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

require 'tempfile'

module AudioUtils
  def convert_to_wav(file)
    FileUtils.mv file, "#{file}.mp3"
    `lame --decode #{file}.mp3 #{file}.wav`
    File.delete "#{file}.mp3"
    FileUtils.mv "#{file}.wav", file
  end

  def convert_to_8000_hz_gsm(input, output)
    new_input = File.is_wav?(input) ? "#{input}.wav" : "#{input}.gsm"
    FileUtils.mv input, new_input
    FileUtils.makedirs File.dirname(output)
    `sox #{new_input} -r 8000 -c1 #{output}`
    FileUtils.mv new_input, input
    if $?.exitstatus == 2
      raise Exception.new 'Error processing audio file'
    end
  end

  def download_url_to(url, target_path)
    download_url_to_temporary_location(url) do |file|
      convert_to_wav file if File.is_mpeg? file
      convert_to_8000_hz_gsm file, target_path
    end
  end

  def download_url_to_temporary_location(url)
    tmp_file = Tempfile.new "url", Rails.root.join('tmp')
    tmp_file.binmode

    http = EventMachine::HttpRequest.new(url).get
    http.stream { |chunk| tmp_file.print chunk }

    f = Fiber.current
    http.callback do
      begin
        if http.response_header.status.to_i != 200
          raise "Download failed with status #{http.response_header.status}"
        end

        tmp_file.flush

        yield tmp_file.path

        File.delete tmp_file
        f.resume
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error) }
    Fiber.yield
  end
end