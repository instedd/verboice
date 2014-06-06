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

class ApiController < ApplicationController
  before_filter :authenticate_api_account!
  skip_before_filter :verify_authenticity_token

  def errors_to_json(model_object, action)
    attrs = {
      :summary => "There were problems #{action} the #{model_object.class.model_name}",
      :properties => []
    }
    model_object.errors.each do |name, value|
      attrs[:properties] << { name => value }
    end
    attrs
  end
end
