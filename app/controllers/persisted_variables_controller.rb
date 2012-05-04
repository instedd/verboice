class PersistedVariablesController < ApplicationController
  before_filter :authenticate_account!

  def index
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variables = @contact.persisted_variables

    respond_to do |format|
      format.html
      format.json { render json: @persisted_variables }
    end
  end

  def show
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variable = @contact.persisted_variables.find(params[:id])

    respond_to do |format|
      format.html
      format.json { render json: @persisted_variable }
    end
  end

  def new
    @persisted_variable = PersistedVariable.new
    @persisted_variable.contact = current_account.contacts.find(params[:contact_id])

    respond_to do |format|
      format.html
      format.json { render json: @persisted_variable }
    end
  end

  def edit
    @persisted_variable = PersistedVariable.find(params[:id])
    @contact = @persisted_variable.contact
  end

  def create
    @persisted_variable = PersistedVariable.new(params[:persisted_variable])
    @persisted_variable.account = current_account
    @persisted_variable.contact = current_account.contacts.find(params[:contact_id])

    respond_to do |format|
      if @persisted_variable.save
        format.html { redirect_to [@persisted_variable.contact, @persisted_variable], notice: 'Persisted variable was successfully created.' }
      else
        format.html { render action: "new" }
      end
    end
  end

  def update
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variable = @contact.persisted_variables.find(params[:id])

    respond_to do |format|
      if @persisted_variable.update_attributes(params[:persisted_variable])
        format.html { redirect_to [@contact, @persisted_variable], notice: 'Persisted variable was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @persisted_variable.errors, status: :unprocessable_entity }
      end
    end
  end

  def destroy
    @contact = current_account.contacts.includes(:persisted_variables).find(params[:contact_id])
    @persisted_variable = @contact.persisted_variables.find(params[:id])
    @persisted_variable.destroy

    respond_to do |format|
      format.html { redirect_to contact_persisted_variables_path(@contact) }
      format.json { head :no_content }
    end
  end
end
