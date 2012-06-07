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

class Commands::RetrieveVariableCommand < Command

  attr_accessor :variable_name

  def initialize variable_name
    @variable_name = variable_name
  end

  def run session
    session.trace "Retrieving '#{@variable_name}'.", command: 'retrieve_variable', action: 'start'
    account = session.call_log.account
    contact = account.contacts.find_by_address(session.address.presence || "Anonymous#{session.call_log.id}" )

    if contact && (persisted_variable = contact.persisted_variables.find_by_name @variable_name)
        set_value_to persisted_variable.value, session
      unless session.address.presence
        session.trace "Caller address is unknown. For current call, variable '#{@variable_name}' has been retrieved from contact '#{contact.address}'.", command: 'retrieve_variable', action: 'retrieve'
      end
    else
      set_value_to_nil_for session
      unless session.address.presence
        session.error "Caller address is unknown. Variable '#{@variable_name}' can't be retrieved for an anonymous contact.", command: 'retrieve_variable', action: 'retrieve'
      end
    end
    session.trace "'#{@variable_name}' retrieved.", command: 'retrieve_variable', action: 'finish'
    super
  end

  def set_value_to_nil_for session
    set_value_to nil, session
  end

  def set_value_to value, session
    Commands::AssignCommand.new("var_#{@variable_name}", value).run(session)
  end
end