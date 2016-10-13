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

class ContactsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project
  before_filter :load_filters, :only => :index
  before_filter :load_sorting, :only => :index
  before_filter :initialize_context, :only => [:show, :edit, :update, :destroy]
  before_filter :check_project_admin, :only => [:create, :edit, :update, :destroy]
  before_filter :init_calls_context, :only => [:calls, :queued_calls]

  def index
    @page = params[:page] || 1
    @contacts = ContactsFinder.for(@project).find(@filters, includes: [:addresses, :recorded_audios, :persisted_variables, :project_variables], sorting: @sorting)
    @project_variables = @project.project_variables
    @fields = ContactsFinder.contact_fields
    @implicit_variables = ImplicitVariable.subclasses

    respond_to do |format|

      format.html do
        @contacts = @contacts.page(@page).per(15)
        load_recorded_audio_descriptions
      end

      format.js do
        @contacts = @contacts.page(@page).per(15)
        load_recorded_audio_descriptions
      end

      format.json do
        render json: @contacts
      end

      format.csv do
        @stats = ContactStats.for @project
      end

    end
  end

  def new
    @contact = Contact.new
    @project_variables = @project.project_variables
    load_empty_variables(@contact)
    @persisted_variables = @contact.persisted_variables

    @contact.project = @project

    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @contact }
    end
  end

  def edit
    load_empty_variables(@contact)
    @persisted_variables = @contact.persisted_variables
  end

  def create
    mark_empty_variables_for_removal params

    @contact = Contact.new(params[:contact])
    @contact.project = @project

    respond_to do |format|
      if @contact.save
        format.html { redirect_to project_contacts_url(@project), notice: 'Contact was successfully created.' }
        format.json { render json: @contact, status: :created, location: @contact }
      else
        load_empty_variables(@contact)
        format.html { render action: "new" }
        format.json { render json: @contact.errors, status: :unprocessable_entity }
      end
    end
  end

  def update
    mark_empty_variables_for_removal params
    respond_to do |format|
      if @contact.update_attributes(params[:contact])
        format.html { redirect_to project_contacts_url(@project), notice: 'Contact was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @contact.errors, status: :unprocessable_entity }
      end
    end
  end

  def destroy
    @contact.destroy
    respond_to do |format|
      format.html { redirect_to project_contacts_url(@project) }
      format.json { head :no_content }
    end
  end

  def calls
    @logs = @project.call_logs.includes(:channel, :schedule)
      .where(contact_id: @contact.id)
      .order('id DESC')
      .page(@page).per(@per_page)
  end

  def queued_calls
    @calls = @project.queued_calls.includes(:channel, :call_log, :schedule)
      .where(address: @contact.addresses.map(&:address))
      .order('id DESC')
      .page(@page).per(@per_page)
  end

  def upload_csv
    @importer = ContactsImporter.new current_account, @project

    error = @importer.save_csv(params[:file])
    if error
      flash[:alert] = error
      redirect_to project_contacts_path(@project)
    else
      @column_specs = @importer.guess_column_specs
      @variables = []
      @importer.project_variables.each do |var|
        @variables.push id: var.id, name: var.name
      end
      @importer.implicit_variables.map do |var|
        @variables.push id: var.key, name: var.key
      end
      @variables.sort_by! { |var| var[:name].downcase }
    end
  end

  def import_csv
    @importer = ContactsImporter.new current_account, @project
    @importer.column_specs = JSON.parse(request.body.read)['column_specs']
    result = @importer.import
    flash.notice = "Import successful. Created: #{result[:created]}, Updated: #{result[:updated]}, Unchanged: #{result[:unchanged]}"
    render json: :ok
  end

  private

  def load_empty_variables(contact)
    ImplicitVariable.subclasses.each do |implicit_variable|
      unless contact.persisted_variables.any? { |persisted| persisted.implicit_key == implicit_variable.key }
        contact.persisted_variables << PersistedVariable.new(:implicit_key => implicit_variable.key)
      end
    end

    @project.project_variables.each do |project_variable|
      unless contact.persisted_variables.any? { |persisted| persisted.project_variable == project_variable }
        contact.persisted_variables << PersistedVariable.new(project_variable: project_variable)
      end
    end
  end

  def initialize_context
    @contact = @project.contacts.includes(:addresses).includes(:recorded_audios).includes(:persisted_variables).find(params[:id])
    @recorded_audios = @contact.recorded_audios
    @persisted_variables = @contact.persisted_variables
    @project_variables = @project.project_variables
  end

  def mark_empty_variables_for_removal params
    params[:contact][:persisted_variables_attributes].each do |index, variable|
      unless variable['value'].present?
        variable['_destroy'] = "1"
      end
    end if params[:contact][:persisted_variables_attributes].present?
  end

  def load_filters
    @filters = params[:filters_json].present? ? JSON.parse(params[:filters_json]) : []
  end

  def load_sorting
    @sorting = if params[:sort_type].presence
      { params[:sort_type] => params[:sort_key], :direction => (params[:sort_dir] == 'down' ? 'DESC' : 'ASC')  }.with_indifferent_access
    end
  end

  def init_calls_context
    @contact = @project.contacts.find(params[:id])
    @page = params[:page] || 1
    @per_page = 10
  end

  def load_recorded_audio_descriptions
    @recorded_audio_descriptions = RecordedAudio.select(:description).where(:contact_id => @contacts.collect(&:id)).collect(&:description).to_set
  end
end
