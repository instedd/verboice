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

class VrzContainer

  def initialize(project)
    @project = project
    @recoding_manager = RecordingManager.for(@project)
  end

  def self.for(project)
    self.new(project)
  end

  def import path
    Zip::ZipFile.open(path) do |zip|
      zip.each do |entry|
        ext = File.extname entry.name
        case ext
        when '.yml'
          @project.user_flow = YAML::load(zip.read(entry))
        when '.wav'
          zip.extract(entry, File.join(@recoding_manager.recordings_folder, entry.name)) {true} # true to always overwrite
        end
      end
    end
  end

  def export path
    Zip::ZipOutputStream.open(path) do |zos|
      # workflow
      zos.put_next_entry 'workflow.yml'
      zos.print @project.user_flow.to_yaml
      # audio files
      Dir.glob(File.join(@recoding_manager.recordings_folder, '*.wav')) do |audio_file|
        zos.put_next_entry File.basename(audio_file)
        zos.print IO.read(audio_file)
      end
    end
  end

end