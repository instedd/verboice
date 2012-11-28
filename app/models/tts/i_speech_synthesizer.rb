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
  def initialize(options = {})
    @api_key = options[:api_key]
  end

  def synth(text, voice, target_path)
    url = "http://api.ispeech.org/api/rest?apikey=#{@api_key}&action=convert&voice=#{voice}&format=mp3&text=#{CGI.escape text}"
    download_url_to url, target_path
  end

  def voices
  	@voices ||= load_voices
  end

  private

  def load_voices
  	ispeech_voices = JSON.parse RestClient.get('http://api.ispeech.org/api/rest?apikey=developerdemokeydeveloperdemokey&action=information&output=json')
    tmp_voices = Hash.new { |h, k| h[k] = {} }
  	ispeech_voices.each do |key, value|
      if key =~ /voice-(\d+)/
        tmp_voices[$1][:id] = value
      elsif key =~ /voice-locale-(\d+)/
        tmp_voices[$1][:locale] = value[0..1]
      elsif key =~ /voice-description-(\d+)/
        tmp_voices[$1][:description] = value
      end
  	end


    voices = Hash.new { |h, k| h[k] = [] }
    tmp_voices.values.each do |voice|
      voices[voice[:locale]] << voice.except(:locale)
    end

    voices
  end
end