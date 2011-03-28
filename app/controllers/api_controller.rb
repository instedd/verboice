class ApiController < ApplicationController
  before_filter :authenticate_account!

  def call
    if params[:application]
      application = current_account.applications.find params[:application]
    elsif params[:callback]
      application = current_account.applications.find_by_callback_url params[:callback]
      if not application
        application = Application.create!(
          :name => params[:callback],
          :callback_url => params[:callback],
          :account => current_account
        )
      end
    end
    address = params[:address]

    call_log = application.call address

    render :json => {:call_id => call_log.id, :state => call_log.state}
  end
end
