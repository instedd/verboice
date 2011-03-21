class ApiController < ApplicationController
  before_filter :authenticate_account!

  def call
    @application = current_account.applications.find params[:application]
    @address = params[:address]

    resp = @application.call @address

    render :json => resp
  end
end
