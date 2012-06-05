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

class Commands::PersistVariableCommand < Command

  attr_accessor :variable_name, :expression

  def initialize variable_name, expression
    @variable_name = variable_name
    @expression    = expression
  end

  def run session
    contact            = contact_from session
    persisted_variable = contact.persisted_variables.find_by_name @variable_name

    if persisted_variable
      persisted_variable.value = evaluate_expression(session)
      persisted_variable.save!
    else
      contact.persisted_variables.create!\
        name: @variable_name,
        value: evaluate_expression(session)
    end
    super
  end

  def evaluate_expression(session)
    if @expression
      session.eval(@expression)
    else
      nil
    end
  end

  def contact_from session

    account = session.call_log.account

    contact = if session.address.presence
      account.contacts.where(:address => session.address).first_or_create!
    else
      account.contacts.where(:address => "Anonymous#{session.call_log.id}", :anonymous => true).first_or_create!
    end
    session.trace "Caller address is unknown. Variable '#{@variable_name}' saved for contact #{contact.address}." unless session.address.presence

    contact
  end
end