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

class TTS::SystemSynthesizer < TTS::Synthesizer
  if RUBY_PLATFORM =~ /darwin/
    def self.instance
      TTS::MacSynthesizer.new
    end
  else
    def self.instance
      TTS::FestivalSynthesizer.new
    end
  end

  def synth(text, voice, target_path, options = {})
    wav_file = "#{target_path}.wave"

    if is_available?
      say = IO.popen command_for(voice, wav_file), 'w'
      say.write text
      say.close

      if options[:convert_to_gsm] == false
        FileUtils.mv wav_file, target_path
      else
        convert_to_8000_hz_gsm wav_file, target_path
      end
    else
      raise "No available TTS engine. Can't execute the console command: #{command_name}"
    end
  ensure
    File.delete wav_file rescue nil
  end
end