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

module Voxeo
  class UrlHelper

    def self.audio_url(key, params = {})
      url "/audio/#{key}", params
    end

    def self.callback_url(params = {})
      url '/', params
    end

    def self.url(path = '/', params = {})
      fallback_host = params.delete(:host)
      qs = params.to_a.map{|x|x.join('=')}.join('&')
      if host.present? && port.present?
        url = "#{host}:#{port}"
      elsif host.present?
        url = host
      else
        url = fallback_host
      end
      url = "http://#{url}#{path}"
      url = "#{url}?#{qs}" if qs.present?
      url
    end

    def self.config
      Rails.configuration.voxeo_configuration[:http_url_options]
    end

    def self.host
      config[:host]
    end

    def self.port
      config[:port]
    end
  end
end