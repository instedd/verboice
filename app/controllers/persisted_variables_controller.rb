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
