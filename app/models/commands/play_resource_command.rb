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

    #TODO: Check language and resource kind and run the appropiate command
    text = Resource.find(@resource_id).localized_resources.first.text
    Commands::SayCommand.new(text).run(session)

    #TODO: Change the logs to distinguish if is play or say and to log the text or the audio name instead of the resource id
    session.info "Play Resource '#{@resource_id}' finished.", command: 'play_resource', action: 'finish'
    super
  end

  def capture_option_name
    #TODO: Check language and resource kind and return the appropiate option name
    :say
  end

  def capture_resource
    #TODO: Check language and resource kind and return the appropiate text or file path
    Resource.find(@resource_id).localized_resources.first.text
  end
end
