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
module Commands
  class PlayResourceCommand < Command

    def initialize(resource_guid, language=nil)
      @resource_guid = resource_guid
      @language = language
    end

    def run(session)
      session.info "Play Resource: '#{@resource_guid}'", command: 'play_resource', action: 'start'
      resource = localized_resource(session)

      resource.play_command_for(self).run(session)

      session.info "Play Resource '#{@resource_guid}' finished.", command: 'play_resource', action: 'finish'
      super
    end

    def capture_resource_hash session
      resource = localized_resource(session)

      { capture_option_name(resource) => capture_resource(session, resource) }
    end

    def play_url_command_for resource
      PlayUrlCommand.new(resource.url)
    end

    def play_upload_command_for resource
      play_audio_command_for resource
    end

    def play_text_command_for resource
      SayCommand.new(resource.text)
    end

    def play_record_command_for resource
      play_audio_command_for resource
    end

    def play_audio_command_for resource
      PlayAudioCommand.new(resource)
    end

    def url_capture_resource_for resource, session
      PlayUrlCommand.new(resource.url).download(session)
    end

    def upload_capture_resource_for resource, session
      play_capture_resource_for resource, session
    end

    def text_capture_resource_for resource, session
      resource.text
    end

    def record_capture_resource_for resource, session
      play_capture_resource_for resource, session
    end

    def play_capture_resource_for resource, session
      PlayAudioCommand.new(resource).download(session)
    end

    def localized_resource(session)
      language = @language.presence || session.language

      # TODO: basta demeter puntos
      session.call_flow.project.resources.find_by_guid(@resource_guid).available_resource_for(language)
    end

    def capture_resource session, resource
      resource.capture_resource_for self, session
    end

    def capture_option_name resource
      if resource.is_a? TextLocalizedResource
        :say
      else
        :play
      end
    end
  end
end
