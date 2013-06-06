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

class Commands::NuntiumCommand < Command

  def initialize(resource_guid, rcpt_type, options = {})
    @resource_guid = resource_guid
    @rcpt_type = rcpt_type
    # options should contain
    # - rcpt_address if rcpt_type is '3rdparty'
    # - rcpt_variable if rcpt_type is 'variable'
    # - language to override localization
    @options = options
  end

  def run(session)
    session.info "Send text message '#{@resource_guid}'", command: 'nuntium', action: 'start'
    # FIXME
    # - determine the address of the recipient of the message
    # - extract the text to send from the resource (using localized resource)
    # - send the message through Nuntium
    session.info "Send text message '#{@resource_guid}' finished", command: 'nuntium', action: 'finish'
    super
  end

end
