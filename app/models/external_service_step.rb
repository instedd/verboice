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

class ExternalServiceStep < ActiveRecord::Base
  belongs_to :external_service

  has_one :project, through: :external_service
  attr_accessible :callback_url, :display_name, :icon, :name, :kind, :variables, :response_variables, :session_variables, :guid, :script, :external_service_id, :async

  serialize :variables, Array
  serialize :response_variables, Array
  serialize :session_variables, Array

  validates :name, :presence => true, :uniqueness => { :scope => :external_service_id }

  validates :guid, :presence => true, :uniqueness => { :scope => :external_service_id }

  validate :validate_variables

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def validate_variables
    variables.each{|v| v.valid?(self, :variables)}
    response_variables.each{|v| v.valid?(self, :response_variables)}
    true
  end

  class Variable < Struct.new(:name, :display_name, :type)
    def valid?(parent, field)
      unless self.name =~ /^[a-zA-Z_][a-zA-Z0-9_]*$/
        parent.errors.add(field, "contain invalid name #{self.name}")
      end
    end
  end
end
