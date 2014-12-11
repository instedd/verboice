class HubController < ApplicationController
  before_filter :authenticate_account!

  def api
    api = hub_api
    uri = "api/" + params[:path]
    render json:api.json(uri)
  end

  private

  def hub_api
    HubClient::Api.trusted(current_account.email)
  end
end
