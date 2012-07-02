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

class PersistedVariable < ActiveRecord::Base
  has_one :project, :through => :project_variable
  belongs_to :contact, :inverse_of => :persisted_variables
  belongs_to :project_variable, :inverse_of => :persisted_variables


  validates_presence_of :contact, :project_variable
  attr_accessible :contact, :value, :project_variable, :project_variable_id

  def typecasted_value
    if value && value =~ /^[-+]?[0-9]+$/
      value.to_i
    else
      value
    end
  end
end
