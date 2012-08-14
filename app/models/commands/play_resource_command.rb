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

      if resource.is_an? UrlLocalizedResource
        PlayUrlCommand.new(resource.url).run(session)
      elsif resource.is_a? RecordLocalizedResource
        PlayAudioCommand.new(resource).run(session)
      elsif resource.is_a? TextLocalizedResource
        SayCommand.new(resource.text).run(session)
      end

      session.info "Play Resource '#{@resource_guid}' finished.", command: 'play_resource', action: 'finish'
      super
    end

    def capture_resource session
      resource = localized_resource(session)

      { capture_option_name(resource) => resource_command(session, resource) }
    end

  private

    def localized_resource(session)
      if @language.present?
        language = @language
      else
        var_name = ImplicitVariables::Language.key

        RetrieveVariableCommand.new(var_name).run(session) unless session["var_#{var_name}"].present?

        language = session["var_#{var_name}"]
      end

      session.call_flow.project.resources.find_by_guid(@resource_guid).available_resource_for(language)
    end

    def resource_command session, resource
      if resource.is_an? UrlLocalizedResource
        PlayUrlCommand.new(resource.url).download(session)
      elsif resource.is_a? RecordLocalizedResource
        PlayAudioCommand.new(resource).download(session)
      elsif resource.is_a? TextLocalizedResource
        resource.text
      end
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
