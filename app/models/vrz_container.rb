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

  def initialize(call_flow, export_audios = true)
    @call_flow = call_flow
    @recording_manager = RecordingManager.for(@call_flow)
    @export_audios = export_audios
  end

  def self.for(call_flow, export_audios=true)
    self.new(call_flow, export_audios)
  end

  def import path
    Zip::ZipFile.open(path) do |zip|
      zip.each do |entry|
        ext = File.extname entry.name
        case ext
        when '.yml'
          if entry.name == 'workflow.yml'
            @call_flow.user_flow = YAML::load(zip.read(entry))
          else
            if entry.name.split[0] == 'Service'
              attrs = YAML::load(zip.read(entry))
              e = ExternalService.find_by_guid(attrs['guid'])
              unless e
                e = ExternalService.new attrs
                e.project = @call_flow.project
                e.save!
              end
            elsif entry.name.split[0] == 'Step'
              attrs = YAML::load(zip.read(entry))
              e = ExternalServiceStep.find_by_guid(attrs['guid'])
              unless e
                e = ExternalServiceStep.new attrs
                e.save!
              end
            end
          end
        when '.wav'
          zip.extract(entry, File.join(@recording_manager.recordings_folder, entry.name)) {true} # true to always overwrite
        end
      end
    end
    @call_flow.save!
  end

  def export path
    Zip::ZipOutputStream.open(path) do |zos|
      # workflow
      zos.put_next_entry 'workflow.yml'
      zos.print @call_flow.user_flow.to_yaml
      @call_flow.external_service_guids.each do |external_service_guid|
        service = ExternalService.find_by_guid(external_service_guid)
        zos.put_next_entry "Service #{service.guid}.yml"
        zos.print(service.attributes.tap do |a|
          a.delete 'id'
          a.delete 'project_id'
        end.to_yaml)
        service.external_service_steps.each do |step|
          zos.put_next_entry "Step #{step.guid}.yml"
          zos.print step.attributes.tap{ |a| a.delete 'id' }.to_yaml
        end
      end
      # audio files
      if @export_audios
        Dir.glob(File.join(@recording_manager.recordings_folder, '*.wav')) do |audio_file|
          zos.put_next_entry File.basename(audio_file)
          zos.print IO.read(audio_file)
        end
      end
    end
  end

end