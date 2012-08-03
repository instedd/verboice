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
    audios = []
    Zip::ZipFile.open(path) do |zip|
      zip.each do |entry|
        unless entry.name.scan(/\__MACOSX*/).present?
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
              elsif entry.name.split[0] == 'resource'
                attrs = YAML::load(zip.read(entry))
                resource = Resource.find_by_guid(attrs['guid'])
                unless resource
                  resource.update_attributes! attrs
                else
                  resource = Resource.new attrs
                  resource.project = @call_flow.project
                  resource.save!
                end
              elsif entry.name.split[0] == 'localized_resource'
                attrs = YAML::load(zip.read(entry))
                localized_resource = LocalizedResource.find_by_guid(attrs['guid'])
                if localized_resource
                  localized_resource.update_attributes! attrs
                else
                  localized_resource = LocalizedResource.new attrs
                  localized_resource.save!
                end
              end
            end
          when '.wav'
            #Wait until all the resources are loaded to include the audios
            audios << entry
          end
        end
      end
      audios.each do |entry|
        guid = File.basename(entry.name).split.last.gsub('.wav', '')
        localized_resource = LocalizedResource.find_by_guid(guid)
        if localized_resource
          localized_resource.audio= zip.read(entry)
          localized_resource.save!
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

      #external services
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
        #TODO Change this to export only the resources used in the call flow
        @call_flow.project.resources.each do |resource|
          resource.localized_resources.each do |localized_resource|
            zos.put_next_entry "localized_resource #{resource.guid} - #{localized_resource.language} - #{localized_resource.guid}.yml"
            zos.print(localized_resource.attributes.tap do |attributes|
                        attributes.delete 'audio'
                        attributes.delete 'id'
                      end.to_yaml) #the audio is set to nil to avoid dumping the wav inside the yaml
            if (localized_resource.is_a?(RecordLocalizedResource) || localized_resource.is_a?(UploadLocalizedResource)) && localized_resource.audio.present?
              zos.put_next_entry "resource_audio #{resource.guid} - #{localized_resource.language} - #{localized_resource.guid}.wav"
              zos.print localized_resource.audio
            end
          end
          zos.put_next_entry "resource #{resource.guid}.yml"
          zos.print(resource.attributes.tap do |attributes|
                      attributes.delete 'id'
                      attributes.delete 'project_id'
                    end.to_yaml)
        end
      end
    end
  end
end
