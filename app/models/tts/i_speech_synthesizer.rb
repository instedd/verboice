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

class TTS::ISpeechSynthesizer < TTS::Synthesizer
  AUDIO_FORMAT = Rails.configuration.verboice_configuration[:ispeech_format] || "mp3"

  def initialize(options = {})
    @api_key = options[:api_key]
  end

  def synth(text, voice, target_path, options = {})
    url = "http://api.ispeech.org/api/rest?apikey=#{@api_key}&action=convert&voice=#{voice}&format=#{AUDIO_FORMAT}&text=#{CGI.escape text}"
    download_url_to url, target_path
  end

  def voices
  	@voices ||= load_voices
  end

  private

  def load_voices
  	YAML.load_file File.expand_path("../i_speech_voices.yml", __FILE__)
  end
end