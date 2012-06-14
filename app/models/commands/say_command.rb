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

class Commands::SayCommand < Command
  attr_accessor :text

  def initialize(text)
    @text = text
  end

  def run(session)
    session.info "Say '#{@text}'", command: 'say', action: 'start'
    session.pbx.say @text, if_hang_up: lambda() { |offset| session.info "User interrupted playback at #{offset} milliseconds.", command: 'say', action: 'user_hang_up' }
    session.info "Say '#{@text}' finished.", command: 'say', action: 'finish'
    super
  end
end
