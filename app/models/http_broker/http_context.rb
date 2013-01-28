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

require 'cgi'

module HttpBroker
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