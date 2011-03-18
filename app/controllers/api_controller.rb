class ApiController < ApplicationController
  before_filter :authenticate_account!

  def call
    @application = current_account.applications.find params[:application]
    @address = params[:address]

    client = EM.connect '127.0.0.1', 8787, MagicObjectProtocol::Client
    client.call @address, @application.id
    head :ok
  end
end
