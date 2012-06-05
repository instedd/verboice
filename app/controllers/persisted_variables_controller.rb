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

class PersistedVariablesController < ApplicationController
  before_filter :authenticate_account!

  before_filter :load_persisted_variable_and_contact, only: [:show, :edit, :update, :destroy]

  def index
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variables = @contact.persisted_variables
  end

  def show
  end

  def new
    @persisted_variable = PersistedVariable.new
    @persisted_variable.contact = current_account.contacts.find(params[:contact_id])
  end

  def edit
  end

  def create
    @persisted_variable = PersistedVariable.new(params[:persisted_variable])
    @persisted_variable.account = current_account
    @persisted_variable.contact = current_account.contacts.find(params[:contact_id])

    if @persisted_variable.save
      redirect_to [@persisted_variable.contact, @persisted_variable], notice: 'Persisted variable was successfully created.'
    else
      render action: "new"
    end
  end

  def update
    if @persisted_variable.update_attributes(params[:persisted_variable])
      redirect_to [@contact, @persisted_variable], notice: 'Persisted variable was successfully updated.'
    else
      render action: "edit"
    end
  end

  def destroy
    @persisted_variable.destroy
    redirect_to contact_persisted_variables_path(@contact)
  end

  private

  def load_persisted_variable_and_contact
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variable = @contact.persisted_variables.find(params[:id])
  end
end
