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