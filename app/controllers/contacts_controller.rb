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
  before_filter :initialize_context, :only => [:show, :edit, :update, :destroy]
  before_filter :check_project_admin, :only => [:create, :edit, :update, :destroy]

  def index
    @contacts = @project.contacts.includes(:addresses).includes(:recorded_audios).includes(:persisted_variables).includes(:project_variables)
    @project_variables = @project.project_variables
    @recorded_audio_descriptions = RecordedAudio.select(:description).where(:contact_id => @contacts.collect(&:id)).collect(&:description).to_set
    @implicit_variables = ImplicitVariable.subclasses

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @contacts }
    end
  end

  def new
    @contact = Contact.new

    ImplicitVariable.subclasses.each do |implicit_variable|
      @contact.persisted_variables << PersistedVariable.new(:implicit_key => implicit_variable.key)
    end

    @project_variables = @project.project_variables
    @project_variables.each do |project_variable|
      @contact.persisted_variables << PersistedVariable.new(project_variable: project_variable)
    end
    @persisted_variables = @contact.persisted_variables

    @contact.project = @project

    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @contact }
    end
  end

  def edit
    ImplicitVariable.subclasses.each do |implicit_variable|
      unless @contact.persisted_variables.any? { |persisted| persisted.implicit_key == implicit_variable.key }
        @contact.persisted_variables << PersistedVariable.new(:implicit_key => implicit_variable.key)
      end
    end

    @project_variables.each do |project_variable|
      unless @contact.persisted_variables.any? { |persisted| persisted.project_variable == project_variable }
        @contact.persisted_variables << PersistedVariable.new(project_variable: project_variable)
      end
    end
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

  private

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
end
