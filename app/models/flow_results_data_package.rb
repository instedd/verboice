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
        "name" => "#{name}_responses",
        "path" => "#{data_package_uri}/responses",
        "api-data-url" => "#{data_package_uri}/responses",
        "mediatype" => "application/json",
        "encoding" => "utf-8",
        "schema" => floip_schema
      }]
    }
  end

  def name
    "call_flow_#{call_flow.id}_since_#{created_at.strftime("%Y-%m-%d")}"
  end

  def floip_schema
    {
      "fields" => [
        field("timestamp", "Timestamp", "datetime"),
        field("row_id", "Row ID", "string"),
        field("contact_id", "Contact ID", "string"),
        field("question_id", "Question ID", "string"),
        field("response", "Response", "any"),
        field("response_metadata", "Response Metadata", "object")
      ],
      "questions" => FlowResultsDataPackage.schema_questions(questions())
    }
  end

  def questions
    self.call_flow.user_flow
      .map{|step| ::FlowResults::Question.from_step(step)}
      .compact
  end

  def self.schema_questions(questions)
    questions.map(&:to_h).reduce({}, :merge)
  end

  private

  def field(name, title, type)
    { "name" => name, "title" => title, "type" => type}
  end
end