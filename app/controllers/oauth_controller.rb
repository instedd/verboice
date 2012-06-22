# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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