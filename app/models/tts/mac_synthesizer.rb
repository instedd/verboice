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

class TTS::MacSynthesizer < TTS::SystemSynthesizer
  def command_for(voice, wav_file)
    cmd = "say"
    cmd << " -v #{voice}" if voice
    cmd << " -o #{wav_file}"
    cmd
  end

  def is_available?
    `which say` && $?.success?
  end

  def voices
  	@voices ||= load_voices
  end

  private

  def load_voices
    say_output = `say -v ?`
  	voices = Hash.new { |h, k| h[k] = [] }
  	say_output.lines.each do |line|
  	  if line =~ /(\w+(\s\w+)*)\s+(\w{2})_\w{2}/
  	    voices[$3] << {id: $1, description: $1}
  	  end
  	end
    voices
  end
end