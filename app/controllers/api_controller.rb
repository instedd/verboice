class ApiController < ApplicationController
  before_filter :authenticate_account!

  def call
    @application = current_account.applications.find params[:application]
    @address = params[:address]

    call_log = @application.call @address

    render :json => {:call_id => call_log.id, :state => call_log.state}
  end
end
