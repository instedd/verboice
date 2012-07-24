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

class Commands::PlayFileCommand < Command
  include Commands::PlayCommand

  def initialize(key)
    @file_id = key
  end

  def run(session)
    session.info "Play file #{@file_id}", command: command_name, action: 'start'
    next_command = super
    session.info "Play file #{@file_id} finished", command: command_name, action: 'finish'
    next_command
  end

  def setup_file(session)
    path = get_target_path(session)
    convert_to_8000_hz_gsm file_to_convert_path(session), path
    path
  end

  def should_setup_file?(session, target_path)
    not File.exists?(target_path) or File.mtime(target_path) < File.mtime(file_to_convert_path(session))
  end

  def file_to_convert_path(session)
    session.recording_manager.recording_path_for(@file_id)
  end

  def command_name
    'play_file'
  end
end
