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

class Commands::PlayResourceCommand < Command

  def initialize(resource_id)
    @resource_id = resource_id
  end

  def run(session)
    session.info "Play Resource: '#{@resource_id}'", command: 'play_resource', action: 'start'

    if localized_resource.is_an? UrlLocalizedResource
      Commands::PlayUrlCommand.new(localized_resource.url).run(session)
    elsif localized_resource.is_a? RecordLocalizedResource
      Commands::PlayFileCommand.new(localized_resource.audio).run(session)
    elsif localized_resource.is_a? TextLocalizedResource
      Commands::SayCommand.new(localized_resource.text).run(session)
    end

    session.info "Play Resource '#{@resource_id}' finished.", command: 'play_resource', action: 'finish'
    super
  end

  def capture_option_name
    if localized_resource.is_an? UrlLocalizedResource
      :play
    elsif localized_resource.is_a? RecordLocalizedResource
      :play
    elsif localized_resource.is_a? TextLocalizedResource
      :say
    end
  end

  def capture_resource session
    if localized_resource.is_an? UrlLocalizedResource
      Commands::PlayUrlCommand.new(localized_resource.url).download(session)
    elsif localized_resource.is_a? RecordLocalizedResource
      Commands::PlayFileCommand.new(localized_resource.audio).download(session)
    elsif localized_resource.is_a? TextLocalizedResource
      localized_resource.text
    end
  end

  def localized_resource
    unless @localized_resource
      var_name = ImplicitVariables::Language.key

      RetrieveVariableCommand.new(var_name).run(session) unless session["var_#{var_name}"].present?

      @localized_resource = Resource.find(@resource_id).available_resource_for(session["var_#{var_name}"])
    end
    @localized_resource
  end
end
