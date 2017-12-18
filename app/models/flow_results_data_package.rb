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
require 'json'
class FlowResultsDataPackage < ActiveRecord::Base
  belongs_to :call_flow

  def descriptor(data_package_uri)
    {
      "profile" => "flow-results-package",
      "name" => name,
      "flow-results-specification" => "1.0.0-rc1",
      "created" => created_at,
      "modified" => updated_at,
      "id" => uuid,
      "title" => call_flow.name,
      "resources" => [{
        "path" => "#{data_package_uri}/responses",
        "api-data-url" => "#{data_package_uri}/responses",
        "mediatype" => "application/json",
        "encoding" => "utf-8",
        "schema" => floip_schema,
        "name" => name
      }]
    }.to_json
  end

  def name
    "call_flow_#{call_flow.id}_since_#{created_at.strftime("%Y-%m-%d")}"
  end

  def floip_schema
    {}
  end
end