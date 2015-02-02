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
  before_filter :check_project_admin, :except => [:index, :show_by_address]
  before_filter :check_project_reader, :only => [:index, :show_by_address]

  expose(:project) { @project }

  def index    
    contacts = project.contacts.includes(:addresses).all
    render json: contacts_to_json(contacts)
  end

  def create
    # normalize address parameter in case we get a single address
    if params[:address].present?
      params[:addresses] = [params[:address]]
    end
    params[:addresses] = Array.wrap(params[:addresses])

    contact = project.contacts.build
    params[:addresses].each do |address|
      contact.addresses.build address: address
    end

    project_vars = project.project_variables.all
    project_vars = project_vars.index_by &:name

    (params[:vars] || {}).each do |key, value|
      project_var = project_vars[key]
      unless project_var
        return render text: "No such variable: #{key}", status: :bad_reqeust
      end
      contact.persisted_variables.build project_variable_id: project_var.id, value: value
    end

    if contact.save
      render json: contacts_to_json([contact])[0]
    else
      render json: contact.errors, status: :bad_request
    end
  end

  def show_by_address
    contact = project.contacts.joins(:addresses).where(contact_addresses: {address: params[:address]}).first or return head(:not_found)
    render json: contacts_to_json([contact])[0]
  end

  def update_by_address
    contact = project.contacts.joins(:addresses).where(contact_addresses: {address: params[:address]}).first or return head(:not_found)
    project_vars = project.project_variables.all
    project_vars = project_vars.index_by &:name

    vars = PersistedVariable.includes(:project_variable).where(project_variables: {project_id: project.id}, contact_id: contact.id).all
    vars = vars.index_by { |var| var.project_variable.name }

    Contact.transaction do
      params[:vars].each do |key, value|
        var = vars[key]
        unless var
          project_var = project_vars[key]
          unless project_var
            return render text: "No such variable: #{key}", status: :bad_reqeust
          end

          var = PersistedVariable.new
          var.contact_id = contact.id
          var.project_variable_id = project_var.id
        end
        var.value = value
        var.save!
      end
    end

    render json: contacts_to_json([contact])[0]
  end

  def update_all
    project_vars = project.project_variables.all
    project_vars = project_vars.index_by &:name

    all_vars = PersistedVariable.includes(:project_variable).where(project_variables: {project_id: project.id}).all
    all_vars = all_vars.index_by { |var| [var.project_variable_id, var.contact_id] }

    contacts_ids = project.contacts.pluck(:id)

    Contact.transaction do
      params[:vars].each do |key, value|
        project_var = project_vars[key]
        unless project_var
          return render text: "No such variable: #{key}", status: :bad_reqeust
        end

        # First update all existing variables
        PersistedVariable.update_all({value: value}, {project_variable_id: project_var.id})

        # Now create missing variables (PersistedVariables that don't yet exist for existing contacts)
        contacts_ids.each do |contact_id|
          unless all_vars[[project_var.id, contact_id]]
            var = PersistedVariable.new
            var.project_variable_id = project_var.id
            var.contact_id = contact_id
            var.value = value
            var.save!
          end
        end
      end
    end

    index
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
