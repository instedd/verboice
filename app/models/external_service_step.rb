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

  attr_accessible :callback_url, :display_name, :icon, :name, :kind, :variables

  serialize :variables, Array
  serialize :response_variables, Array

  validates :name, :presence => true, :uniqueness => { :scope => :external_service_id }

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  class Variable < Struct.new(:name, :display_name, :type)
  end

end
