require 'cgi'

module Voxeo
  class HttpContext

    def initialize http_headers, http_query_string
      @http_headers = http_headers
      @http_query_string = http_query_string
    end

    def headers
      @headers ||= init_headers
    end

    def params
      @params ||= init_params
    end

    private

    def init_params
      if @http_query_string
        @http_query_string.split('&').map{|x|x.split('=')}.inject(HashWithIndifferentAccess.new){|r,x|r[x.first]=CGI.unescape(x.second || '');r}
      else
        HashWithIndifferentAccess.new
      end
    end

    def init_headers
      if @http_headers
        @http_headers.split("\x00").map{|x|x.split(':', 2)}.inject(HashWithIndifferentAccess.new){|r,x|r[x.first.strip]=x.second.strip;r}
      else
        HashWithIndifferentAccess.new
      end
    end

  end
end