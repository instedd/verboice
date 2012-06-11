class ContactsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, :only => [:new, :create, :index]
  before_filter :load_contact, :only => [:show, :edit, :update, :destroy]

  def index
    @contacts = @project.contacts

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @contacts }
    end
  end

  def show
    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @contact }
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
        format.html { redirect_to [@project, @contact], notice: 'Contact was successfully created.' }
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
        format.html { redirect_to [@project, @contact], notice: 'Contact was successfully updated.' }
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

  def load_contact
    @contact = current_account.contacts.find(params[:id])
    @project = @contact.project
  end
end
