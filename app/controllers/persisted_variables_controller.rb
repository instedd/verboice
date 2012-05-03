class PersistedVariablesController < ApplicationController
  # GET /persisted_variables
  # GET /persisted_variables.json
  def index
    @persisted_variables = PersistedVariable.all

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @persisted_variables }
    end
  end

  # GET /persisted_variables/1
  # GET /persisted_variables/1.json
  def show
    @persisted_variable = PersistedVariable.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @persisted_variable }
    end
  end

  # GET /persisted_variables/new
  # GET /persisted_variables/new.json
  def new
    @persisted_variable = PersistedVariable.new

    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @persisted_variable }
    end
  end

  # GET /persisted_variables/1/edit
  def edit
    @persisted_variable = PersistedVariable.find(params[:id])
  end

  # POST /persisted_variables
  # POST /persisted_variables.json
  def create
    @persisted_variable = PersistedVariable.new(params[:persisted_variable])
    @persisted_variable.account = current_account

    respond_to do |format|
      if @persisted_variable.save
        format.html { redirect_to @persisted_variable, notice: 'Persisted variable was successfully created.' }
        format.json { render json: @persisted_variable, status: :created, location: @persisted_variable }
      else
        format.html { render action: "new" }
        format.json { render json: @persisted_variable.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /persisted_variables/1
  # PUT /persisted_variables/1.json
  def update
    @persisted_variable = PersistedVariable.find(params[:id])

    respond_to do |format|
      if @persisted_variable.update_attributes(params[:persisted_variable])
        format.html { redirect_to @persisted_variable, notice: 'Persisted variable was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @persisted_variable.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /persisted_variables/1
  # DELETE /persisted_variables/1.json
  def destroy
    @persisted_variable = PersistedVariable.find(params[:id])
    @persisted_variable.destroy

    respond_to do |format|
      format.html { redirect_to persisted_variables_url }
      format.json { head :no_content }
    end
  end
end
