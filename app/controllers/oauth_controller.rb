class OauthController < ApplicationController

  before_filter :load_google_client, :only => [:google, :google_callback]

  def google
    redirect_to @client.auth_code.authorize_url(
      :redirect_uri => google_callback_oauth_url,
      :scope => 'https://www.googleapis.com/auth/fusiontables',
      :access_type => 'offline',
      :approval_prompt => 'force',
      :state => params[:redirect_back_to]
    )
  end

  def google_callback
    begin
      oauth_response = @client.auth_code.get_token(params[:code], :redirect_uri => google_callback_oauth_url)
      current_account.google_oauth_token = OAuthToken.new_from(oauth_response, :google)
      flash[:notice] = "Access for fusion tables was successfully set up"
    rescue Exception => ex
      logger.warn "Error retrieving access token from google: #{ex}"
      flash[:error] = 'Could not obtain access to your fusion tables account'
    end

    redirect_to params[:state].presence || projects_path
  end

  private

  def load_google_client
    @client = OAuth2::Client.google
  end

end