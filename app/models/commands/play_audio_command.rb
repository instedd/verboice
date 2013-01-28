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

class Commands::PlayAudioCommand < Command
  include Commands::PlayCommand

  def initialize(audio_resource)
    @audio_resource = audio_resource
  end

  def run(session)
    session.info "Play audio '#{@audio_resource.description}'", command: command_name, action: 'start'
    next_command = super
    session.info "Play audio '#{@audio_resource.description}' finished", command: command_name, action: 'finish'
    next_command
  end

  def setup_file(session)
    in_temp_dir do |path|
      source_path = File.join(path, "#{@audio_resource.id}")
      File.open(source_path, 'wb') { |f| f.write(@audio_resource.audio) }
      target_path = get_target_path(session)
      convert_to_wav source_path if File.is_mpeg?(source_path)
      convert_to_8000_hz_gsm source_path, target_path
      target_path
    end
  end

  def should_setup_file?(session, target_path)
    not File.exist?(target_path) or File.mtime(target_path) < @audio_resource.updated_at
  end

  def get_target_path(session)
    @target_path ||= session.pbx.sound_path_for @audio_resource.guid
  end

  def command_name
    'play_audio'
  end

  def in_temp_dir

    path = File.expand_path "#{Rails.root}/tmp/data/#{Time.now.to_i}#{rand(1000)}/"
    FileUtils.mkdir_p(path)

    yield(path)

  ensure
    FileUtils.rm_rf(path) if File.exist?(path)
  end
end
