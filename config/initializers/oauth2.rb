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

class OAuth2::Client
  oauth_path = "#{Rails.root}/config/oauth.yml"
  @@oauth_data = File.exists?(oauth_path) ? YAML::load(File.open(oauth_path)) : {}

  def self.google
    api_key, api_secret = self.client_data_for(:google)
    OAuth2::Client.new(api_key, api_secret, :site => 'https://accounts.google.com/', :authorize_url => '/o/oauth2/auth', :token_url => 'o/oauth2/token')
  end

  def self.service_configured?(service)
    @@oauth_data.include?(service.to_s) && !@@oauth_data[service.to_s]['api_key'].blank? && !@@oauth_data[service.to_s]['api_secret'].blank?
  end

  protected

  def self.client_data_for(service)
    raise "OAuth2: Service not supported '#{service}'.\nAvailable services are: #{@@oauth_data.keys.join(', ')}" if not service_configured?(service)
    client_data = @@oauth_data[service.to_s]
    return [client_data['api_key'], client_data['api_secret']]
  end
end