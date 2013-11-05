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

class Commands::CallbackCommand < Command
  attr_accessor :url
  attr_accessor :method
  attr_accessor :params

  def initialize(url = nil, options = {})
    @url = url
    @method = options[:method] || 'post'
    @params = options[:params]
    @response_type = options[:response_type] || :flow
    @variables = options[:variables] || {}
    @external_service_guid = options[:external_service_guid]
    @async = options[:async]
  end

  def serialize_parameters
    {
      url: @url, method: @method, params: @params, response_type: @response_type, variables: @variables,
      external_service_guid: @external_service_guid, async: @async
    }
  end
end
