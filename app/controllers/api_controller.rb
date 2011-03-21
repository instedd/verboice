class ApiController < ApplicationController
  before_filter :authenticate_account!

  def call
    @application = current_account.applications.find params[:application]
    @address = params[:address]

    call_log = CallLog.create! :account => current_account, :application => @application, :state => :active, :details => ''
    call_log.log 'I', "Initiating call from API to #{@address}"
    call_log.save!

    client = EM.connect '127.0.0.1', 8787, MagicObjectProtocol::Client
    begin
      resp = client.call @address, @application.id, call_log.id
    ensure
      client.close_connection
    end

    render :json => resp
  end
end
