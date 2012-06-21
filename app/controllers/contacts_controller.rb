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
  before_filter :load_project, :only => [:new, :create, :index]
  before_filter :initialize_context, :only => [:show, :edit, :update, :destroy]

  def index
    @contacts = @project.contacts.includes(:recorded_audios).includes(:persisted_variables)
    @persisted_variable_names = PersistedVariable.select(:name).where(:contact_id => @contacts.collect(&:id)).collect(&:name).to_set
    @recorded_audio_descriptions = RecordedAudio.select(:description).where(:contact_id => @contacts.collect(&:id)).collect(&:description).to_set

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @contacts }
    end
  end

  def new
    @contact = Contact.new
    @contact.project = @project

    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @contact }
    end
  end

  def edit
  end

  def create
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

  def load_project
    @project = current_account.projects.find(params[:project_id])
  end

  def initialize_context
    @contact = current_account.contacts.includes(:recorded_audios).includes(:persisted_variables).find(params[:id])
    @recorded_audios = @contact.recorded_audios
    @persisted_variables = @contact.persisted_variables
    @project = @contact.project
  end
end
