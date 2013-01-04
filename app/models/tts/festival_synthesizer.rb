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

class TTS::FestivalSynthesizer < TTS::SystemSynthesizer
  def command_for(voice, wav_file)
    cmd = "text2wave"
    cmd << " -o #{wav_file}"
    cmd << " -eval \"(voice.select '#{voice})\"" if voice
    cmd
  end

  def is_available?
    `which text2wave` && $?.success?
  end

  def voices
    @voices ||= load_voices
  end

  private

  def load_voices
    voices = Hash.new { |h, k| h[k] = [] }
    IO.popen 'festival --pipe', 'r+' do |festival|
      festival.write File.read(File.expand_path('../list_festival_voices.scm', __FILE__))
      festival.close_write
      festival.readlines.each do |line|
        parts = line.chomp!.split('|')
        next unless parts.length == 4
        voice, language, gender, dialect = parts
        lang_code = LanguageList::LanguageInfo.find_by_name(language.titleize).iso_639_1
        description = "#{language.titleize} #{gender.titleize} (#{voice})"
        description = "#{dialect.titleize} #{description}" unless dialect == 'none'
        voices[lang_code] << {id: voice, description: description}
      end
    end
    voices
  end
end