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
class Api::ContactsController < ApiController
  expose(:project) { current_account.projects.find params[:project_id] }

  def index
    if params[:address]
      contact = project.contacts.joins(:addresses).where(contact_addresses: {address: params[:address]}).first or return head(:not_found)
      render json: contacts_to_json([contact])[0]
    else
      contacts = project.contacts.includes(:addresses).all
      render json: contacts_to_json(contacts)
    end
  end

  private

  def contacts_to_json(contacts)
    vars = PersistedVariable.includes(:project_variable).where(project_variables: {project_id: project.id}, contact_id: contacts.map(&:id)).all
    vars = vars.group_by(&:contact_id)

    contacts.map do |contact|
      contact_vars = vars[contact.id] || []
      contact_vars = Hash[contact_vars.map { |var| [var.project_variable.name, var.value]}]
      {
        id: contact.id,
        addresses: contact.addresses.map(&:address),
        vars: contact_vars,
      }
    end
  end

end
